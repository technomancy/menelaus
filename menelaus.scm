;; -*- scheme -*-

;; menelaus.scm

(include "keyboard.scm")

(define pressed 0)

(define column-pins (vector 11 12 15 14 10 4 7 8 9 5 6))
(define layout (vector key-a key-s key-d key-f key-g 0
                       key-h key-j key-k key-l key-semicolon))

(define (scan-column pin keycode)
  (if (low? pin)
      (set! pressed keycode)))

(define (loop)
  (set! pressed 0)
  (for 0 10 (lambda (n) (scan-column (vector-ref column-pins n)
                                     (vector-ref layout n))))
  (call-c-func "usb_send" 0 pressed 0 0 0 0 0)
  (loop))

(define (init)
  ;; leonardo pins only go up to 13, but a-star goes to 19
  (set! arduino-ports (vector #x29 #x29 #x29 #x29 #x29 #x26 #x29 #x2C
                              #x23 #x23 #x23 #x23 #x29 #x26
                              #x23 #x23 #x23 #x29 #x2D #x2D))
  (set! arduino-pins (vector 4 8 2 1 16 64 128 64 16 32 64 128 64 128
                             8 2 4 32 65 128))
  (for-each-vector input column-pins)
  (for-each-vector high column-pins) ; activate pullup resistors
  (call-c-func "usb_init")
  (pause 200))

(init)
(loop)

