(include "keycodes.scm")

(define columns (list 0 1 2 3 4 5 6 7 8 9))
(define column-pins (vector 11 12 18 19
                            10 7 8 9 5 6))
(define layout (vector key-a key-s key-d key-f key-g
                       key-h key-j key-k key-l key-semicolon))

(for-each output (list 0 1 2 3))
(for-each high (list 0 1 3))
(low 2)

(for-each-vector input column-pins)
(for-each-vector high column-pins) ; activate pullup resistors

(call-c-func "usb_init")
(pause 200)

(define (scan-column last n)
  (if (low? (vector-ref column-pins n))
      (vector-ref layout n)
      last))

(define (loop)
  (let ((pressed (fold scan-column 0 columns)))
    (call-c-func "usb_send" 0 pressed 0 0 0 0 0))
  (loop))

(loop)
