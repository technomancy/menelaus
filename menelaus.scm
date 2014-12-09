;; -*- scheme -*-

(include "keycodes.scm")

(define column-pins (list 11 12 18 19 10 7 8 9 5 6))
(define layout (list key-a key-s key-d key-f key-g
                     key-h key-j key-k key-l key-semicolon))

(define (scan-column column-pins layout pressed)
  (if (null? column-pins)
      pressed
      (scan-column (cdr column-pins)
                   (cdr layout)
                   (if (low? (car column-pins))
                       (car layout)
                       pressed))))

(define (loop)
  (let ((pressed (scan-row column-pins layout 0)))
    (call-c-func "usb_send" 0 pressed 0 0 0 0 0))
  (loop))

(define (init)
  ;; leonardo pins only go up to 13, but a-star goes to 19
  (set! arduino-ports (vector #x29 #x29 #x29 #x29 #x29 #x26 #x29 #x2C
                              #x23 #x23 #x23 #x23 #x29 #x26
                              #x23 #x23 #x23 #x29 #x2D #x2D))
  (set! arduino-pins (vector 4 8 2 1 16 64 128 64 16 32 64 128 64 128
                             8 2 4 32 65 128))
  (output 1)
  (low 1)
  (for-each input column-pins)
  (for-each high column-pins) ; activate pullup resistors
  (call-c-func "usb_init")
  (pause 200))

(init)
(loop)

