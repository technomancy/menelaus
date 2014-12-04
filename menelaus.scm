;; -*- scheme -*-

;; menelaus.scm

(define (loop)
  (if (low? 11)
      (call-c-func "usb_send" 0 4 0 0 0 0 0)
      (call-c-func "usb_send" 0 0 0 0 0 0 0))
  (loop))

(define (init)
  (input 11)
  (high 11) ; activate pullup resistor
  (call-c-func "usb_init")
  (pause 200))

(init)
(loop)
