(include "keycodes.scm")
(include "layout.scm")

(define rows (list 0 1 2 3))
(define row-pins (vector 3 2 1 0))
(define columns (list 0 1 2 3 4 5 6 7 8 9 10))
(define column-pins (vector 11 12 18 19 10 4 7 8 9 5 6))

(for-each-vector output row-pins)
(for-each-vector high row-pins)
(for-each-vector input column-pins)
(for-each-vector high column-pins) ; activate pullup resistors

(call-c-func "usb_init")
(pause 200)

(define (scan-column last offset)
  (if (low? (vector-ref column-pins (mod offset 11)))
      (vector-ref layout offset)
      last))

(define (scan-row last row)
  (for-each-vector high row-pins)
  (low (vector-ref row-pins row))
  (fold scan-column last (map (lambda (col) (+ col (* row 11))) columns)))

(define (loop)
  (let ((pressed (free! (fold scan-row 0 rows))))
    (call-c-func "usb_send" 0 pressed 0 0 0 0 0))
  (loop))

(loop)
