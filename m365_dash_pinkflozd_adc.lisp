(app-adc-detach 3 1) 

(define min-speed 0.5)
(define brk-minspeed 2)
(define button-safety-speed 4)

(define eco-speed (/ 26 3.6))
(define eco-current 0.6)
(define eco-watts 400)
(define drive-speed (/ 26 3.6))
(define drive-current 0.7)
(define drive-watts 500)
(define sport-speed (/ 26 3.6))
(define sport-current 1.0)
(define sport-watts 700)

(define secret-speed (/ 200 3.6))
(define secret-current 1.0)
(define secret-watts 100000)

(define power-amps (/ 100 (+ (conf-get 'l-in-current-max) 1.5)))
(define min-temp 10)
(define max-temp 60)

(uart-start 115200 'half-duplex)
(gpio-configure 'pin-rx 'pin-mode-in-pu)

(define tx-frame (array-create 14))
(bufset-u16 tx-frame 0 0x55AA)
(bufset-u16 tx-frame 2 0x0821)
(bufset-u16 tx-frame 4 0x6400)

(define uart-buf (array-create type-byte 64))
(define current-speed 0)
(define throttle 0)
(define brake 0)
(define buttonold 0)
(define c-out 0)
(define code 0)

(define presstime (systime))
(define presses 0)

(define off 0)
(define speedmode 4)

(define feedback 0)

(define battery 0)
(define update 0)

(defun adc-input(buffer)
    (progn
        (setvar 'throttle (/(bufget-u8 uart-buf 4) 255.0))
        (setvar 'brake (/(bufget-u8 uart-buf 5) 255.0))

        (if (= off 0)
            (progn
                (if (> current-speed min-speed)
                    (app-adc-override 0 throttle)
                    (app-adc-override 0 0)
                )
                (if (> current-speed brk-minspeed)
                    (app-adc-override 1 brake)
                    (app-adc-override 1 0)
                )
            )
            (progn
                (app-adc-override 0 0)
                (app-adc-override 1 0)
            )
        )
    )
)

(defun update-data()
    (progn

        (if (= off 0)
            (progn
                (if (< (get-temp-fet) max-temp)
                    (bufset-u8 tx-frame 6 speedmode)
                    (bufset-u8 tx-frame 6 (+ 128 speedmode))
                )
                
                (if (> current-speed min-speed)
                    (progn
                        (bufset-u8 tx-frame 10 (to-i current-speed))
                        (bufset-u8 tx-frame 7 (to-i (* power-amps (get-current-in))))
                    )
                    (progn
                        (bufset-u8 tx-frame 10 (to-i battery))
                        (bufset-u8 tx-frame 7 (to-i (*(/(- (get-temp-fet) min-temp) (- max-temp min-temp)) 100)))
                    )
                )
                (bufset-u8 tx-frame 11 (get-fault))
                
            )
            (bufset-u8 tx-frame 6 16)
        )       

    )
)

(defun update-dash(buffer)
    (progn
    
        (if (= update 1)
            (setvar 'update 0)
            (progn
                (update-data)
                (setvar 'update 1)
            )
        )
        
        (if (> feedback 0)
           (progn
               (bufset-u8 tx-frame 9 1)
               (setvar 'feedback (- feedback 1))
           )
           (bufset-u8 tx-frame 9 0)
        )
           
        (setvar 'crc 0)
        (looprange i 2 12
            (setvar 'crc (+ crc (bufget-u8 tx-frame i))))
        (setvar 'c-out (bitwise-xor crc 0xFFFF))
        (bufset-u8 tx-frame 12 c-out)
        (bufset-u8 tx-frame 13 (shr c-out 8))
        (uart-write tx-frame)
    )
)

(defun read-frames()
    (loopwhile t
        (progn
            (uart-read-bytes uart-buf 3 0)
            (if (= (bufget-u16 uart-buf 0) 0x55aa)
                (progn
                    (setvar 'len (bufget-u8 uart-buf 2))
                    (setvar 'crc len)
                    (if (and (> len 0) (< len 10))
                        (progn
                            (uart-read-bytes uart-buf (+ len 4) 0)
                            (looprange i 0 len
                                (setvar 'crc (+ crc (bufget-u8 uart-buf i))))
                            (if (=(+(shl(bufget-u8 uart-buf (+ len 2))8) (bufget-u8 uart-buf (+ len 1))) (bitwise-xor crc 0xFFFF))
                                (handle-frame (bufget-u8 uart-buf 1))
                            )
                        )
                    )
                )
            )
        )
    )
)

(defun handle-frame(code)
    (progn
        (if(= code 0x65)
            (adc-input uart-buf)
        )
        (update-dash uart-buf)
    )
)

(defun handle-button()
    (if (= presses 1)
        (if (= off 1)
            (progn
                (stats-reset)
                (setvar 'off 0)
                (setvar 'feedback 1)
                (if (= speedmode 5)
                    (setvar 'speedmode 4)
                )
                (apply-mode)
            )
            (progn
                (if (= speedmode 1)
                    (progn
                        (setvar 'speedmode 4)
                    )
                    (if (= speedmode 2)
                        (progn
                            (setvar 'speedmode 1)
                        )
                        (if (or (= speedmode 4) (= speedmode 5))
                            (setvar 'speedmode 2)
                        )
                    )
                )
                (apply-mode speedmode)
            )
        )
        (if (and (>= presses 2) (> brake 0.5))
            (progn
                (setvar 'speedmode 5)
                (setvar 'feedback 2)
                (apply-mode speedmode)
            )
        )
    )
)

(defun handle-holding-button()
    (progn
        (if (= off 0)
            (progn
                (setvar 'off 1)
                (setvar 'feedback 1)
            )
        )
    )
)

(defun reset-button()
    (progn
        (setvar 'presstime (systime))
        (setvar 'presses 0)
    )
)

(defun apply-mode()
    (if (= speedmode 1)
        (configure-speed drive-speed drive-watts drive-current)
        (if (= speedmode 2)
            (configure-speed eco-speed eco-watts eco-current)
            (if (= speedmode 4)
                (configure-speed sport-speed sport-watts sport-current)
                (if (= speedmode 5)
                    (configure-speed secret-speed secret-watts secret-current)
                )
            )
        )
    )
)

(defun configure-speed(speed watts current)
    (progn
        (conf-set 'max-speed speed)
        (conf-set 'l-watt-max watts)
        (conf-set 'l-current-max-scale current)
    )
)

(apply-mode)

(defun restart-thread()
    (progn
        (spawn-trap 150 read-frames)
        (recv  ((exit-error (? tid) (? e)) (restart-thread))
        ((exit-ok    (? tid) (? v)) (restart-thread)))
    )
)

(restart-thread)

(loopwhile t
    (progn
        (if (> buttonold (gpio-read 'pin-rx))
            (progn
                (setvar 'presses (+ presses 1))
                (setvar 'presstime (systime))
            )
            (if (> (- (systime) presstime) 2500)
                (if (= (gpio-read 'pin-rx) 0)
                    (if (> (- (systime) presstime) 6000)
                        (progn
                            (if (<= current-speed button-safety-speed)
                                (handle-holding-button)
                            )
                            (reset-button)
                        )
                    )
                    (progn
                        (if (> presses 0)
                            (progn
                                (if (<= current-speed button-safety-speed)
                                    (handle-button)
                                )
                                (reset-button)
                            )
                        )
                    )
                )
            )
        )
        (setvar 'buttonold (gpio-read 'pin-rx))
        
        (setvar 'current-speed (*(get-speed) 3.6))
        (setvar 'battery (*(get-batt) 100))
        
        (sleep 0.1)
    )
)