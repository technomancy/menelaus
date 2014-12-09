(include "keycodes.scm")
(include "layout.scm")

(define row-pins (list 0 1 2 3))
(define column-pins (list 11 12 18 19 10 4 7 8 9 5 6))

(define (scan-column columns row-layout pressed)
  (if (null? columns)
      pressed
      (scan-column (cdr columns)
                   (cdr row-layout)
                   (if (low? (car columns))
                       (car row-layout)
                       pressed))))

(define (scan-rows rows layout-rows pressed)
  (if (null? rows)
      pressed
      (begin
        (for-each high row-pins)
        (low (car rows)) ; activate row
        (scan-rows (cdr rows) (cdr layout-rows)
                   (scan-column column-pins (car layout-rows) 0)))))

(define (loop)
  (let ((pressed (scan-rows row-pins layout 0)))
    (call-c-func "usb_send" 0 pressed 0 0 0 0 0))
  (loop))

(define (init)
  ;; leonardo pins only go up to 13, but a-star goes to 19
  (set! arduino-ports (vector #x29 #x29 #x29 #x29 #x29 #x26 #x29 #x2C
                              #x23 #x23 #x23 #x23 #x29 #x26
                              #x23 #x23 #x23 #x29 #x2D #x2D))
  (set! arduino-pins (vector 4 8 2 1 16 64 128 64 16 32 64 128 64 128
                             8 2 4 32 65 128))
  (for-each output row-pins)
  (for-each input column-pins)
  (for-each high column-pins) ; activate pullup resistors
  (call-c-func "usb_init")
  (pause 200))

(init)
(loop)
