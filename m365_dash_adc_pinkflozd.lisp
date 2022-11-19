(app-adc-detach 3 1)

(uart-start 115200 'half-duplex)
(gpio-configure 'pin-rx 'pin-mode-in-pu)

(define tx-frame (array-create 14))
(bufset-u16 tx-frame 0 0x55AA)
(bufset-u16 tx-frame 2 0x0821)
(bufset-u16 tx-frame 4 0x6400)

(define uart-buf (array-create type-byte 64))
(define throttle 0)
(define brake 0)
(define buttonold 0)
(define c-out 0)

(define presstime (systime))
(define presses 0)

(define off 0)
(define speedmode 2)

(conf-set 'max-speed 0.28)
(conf-set 'l-watt-max 400)

(define feedback 0)

(define current-speed 0)

(defun inp(buffer)
    (progn
        (setvar 'throttle (/(bufget-u8 uart-buf 4) 255.0))
        (setvar 'brake (/(bufget-u8 uart-buf 5) 255.0))

        (if (= off 0)
            (progn
                (app-adc-override 0 throttle)
                (app-adc-override 1 brake)
            )
            (progn
                (app-adc-override 0 0)
                (app-adc-override 1 0)
            )
        )
    )
)

(defun outp(buffer)
    (progn
        (setvar 'crc 0)
        (looprange i 2 12
            (setvar 'crc (+ crc (bufget-u8 tx-frame i))))
        (setvar 'c-out (bitwise-xor crc 0xFFFF)) 
        (bufset-u8 tx-frame 12 c-out)
        (bufset-u8 tx-frame 13 (shr c-out 8))

        (uart-write tx-frame)
    )
)

(defun read-thd()
    (loopwhile t
        (progn
            (uart-read-bytes uart-buf 3 0)
            (if (= (bufget-u16 uart-buf 0) 0x55aa)
                (progn
                    (setvar 'len (bufget-u8 uart-buf 2))
                    (setvar 'crc len)
                    (if (> len 0) 
                        (progn
                            (uart-read-bytes uart-buf (+ len 4) 0)
                            (looprange i 0 len
                                (setvar 'crc (+ crc (bufget-u8 uart-buf i))))
                            (if (=(+(shl(bufget-u8 uart-buf (+ len 2))8) (bufget-u8 uart-buf (+ len 1))) (bitwise-xor crc 0xFFFF))
                                (progn
                                    (if(= (bufget-u8 uart-buf 1) 0x65)
                                        (inp uart-buf))
                                    (outp uart-buf)
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

(spawn 150 read-thd) 

(defun dash-updates()
    (loopwhile t
        (progn
            (setvar 'current-speed (*(get-speed) 3.6))

            (if (= off 1)
                (bufset-u8 tx-frame 6 16)
                (bufset-u8 tx-frame 6 speedmode))

            (bufset-u8 tx-frame 7 (*(get-batt) 100))

            (if (>= (get-temp-fet) 60)
                (bufset-u8 tx-frame 6 128)
            )
        
            (if (> feedback 0)
                (progn
                    (bufset-u8 tx-frame 9 1)
                    (setvar 'feedback (- feedback 1))
                )
                (bufset-u8 tx-frame 9 0))

            (if (> (current-speed) 1)
                (bufset-u8 tx-frame 10 (current-speed))
                (bufset-u8 tx-frame 10 (get-temp-fet)))

            (bufset-u8 tx-frame 11 (get-fault))

            (sleep 0.1)
        )
    )
)

(spawn dash-updates) 

(loopwhile t
    (progn
        (if (<= (current-speed) 1)
        (progn
            (if (> buttonold (gpio-read 'pin-rx))
                (progn
                    (setvar 'presses (+ presses 1))
                    (setvar 'presstime (systime))
                )
                (if (> (- (systime) presstime) 2500) ;
                    (if (= (gpio-read 'pin-rx) 0)
                        (if (> (- (systime) presstime) 6000)
                            (progn
                                (setvar 'off 1)
                                (setvar 'feedback 1)
                                (setvar 'presstime (systime))
                                (setvar 'presses 0)
                            )
                        )
                        (progn
                            (if (= presses 1)
                                (if (= off 1)
                                    (progn
                                        (setvar 'off 0)
                                        (setvar 'feedback 1)
                                    )
                                    (progn
                                        (if (= speedmode 1)
                                            (progn 
                                                (conf-set 'max-speed 0.28)
                                                (conf-set 'l-watt-max 700)
                                                (setvar 'speedmode 4)
                                            )
                                            (if (= speedmode 2)
                                                (progn
                                                    (conf-set 'max-speed 0.28)
                                                    (conf-set 'l-watt-max 500)
                                                    (setvar 'speedmode 1)
                                                )
                                                (if (= speedmode 4)
                                                    (progn
                                                        (conf-set 'max-speed 0.28)
                                                        (conf-set 'l-watt-max 400)
                                                        (setvar 'speedmode 2)
                                                    )
                                                )
                                            )
                                        )
                                    )
                                )
                            )
    
                            (if (>= presses 2)
                                (progn
                                    (if (> brake 0.20)
                                        (progn 
                                            (conf-set 'max-speed 200)
                                            (conf-set 'l-watt-max 150000)
                                            (setvar 'speedmode 4)
                                            (setvar 'feedback 1)
                                        )
                                    )
                                )
                            )

                            (setvar 'presses 0)
                        )
                    )
                )
            )

            (setvar 'buttonold (gpio-read 'pin-rx))
        ))
        (sleep 0.01)
    )
)
