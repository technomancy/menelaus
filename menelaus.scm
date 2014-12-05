;; -*- scheme -*-

(include "keycodes.scm")

(define columns (list 0 1 2 3 4 5 6 7 8 9))
(define column-pins (vector 11 12 18 19 10 7 8 9 5 6))
(define layout (vector key-a key-s key-d key-f key-g
                       key-h key-j key-k key-l key-semicolon))

(define (scan-column last n)
  (if (low? (vector-ref column-pins n))
      (vector-ref layout n)
      last))

(define (loop)
  (let ((pressed (fold scan-column 0 columns)))
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
  (for-each-vector input column-pins)
  (for-each-vector high column-pins) ; activate pullup resistors
  (call-c-func "usb_init")
  (pause 200))

(init)
(loop)

