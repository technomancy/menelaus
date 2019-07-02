(include "keycodes.scm")
(include "layout.scm")

(define rows (list 0 1 2 3))
(define row-pins (vector 3 2 1 0))
(define columns (list 0 1 2 3 4 5 6 7 8 9 10))
(define column-pins (vector 6 5 9 8 7 4 10 19 18 12 11))

(define max-keys 6) ; a single USB frame can only send 6 keycodes plus modifiers


;;; matrix

(define (offset-for row col)
  (+ col (* row (length columns))))

(define (scan-key keys-pressed key-count row col)
  ;; pullup resistors mean a closed circuit is low rather than high
  (if (low? (vector-ref column-pins col))
      (begin
        (if (<= key-count max-keys)
            (vector-set! keys-pressed key-count (offset-for row col))
            #f)
        (+ key-count 1))
      key-count))

(define (scan-column keys-pressed key-count row columns-left)
  (if (= (length columns-left) 0)
      key-count
      (let ((key-count (scan-key keys-pressed key-count
                                 row (car columns-left))))
        (scan-column keys-pressed key-count row (cdr columns-left)))))

(define (activate-row row)
  (for-each-vector high row-pins)
  (low (vector-ref row-pins row)))

(define (scan-matrix keys-pressed key-count rows-left)
  (if (= (length rows-left) 0)
      key-count
      (let ((_ (activate-row (car rows-left)))
            (key-count (scan-column keys-pressed key-count
                                    (car rows-left) columns)))
        (scan-matrix keys-pressed key-count (cdr rows-left)))))


;;; layout

(define (keycode-for keys-pressed which-key keycodes)
  (let ((code (vector-ref layout (vector-ref keys-pressed which-key))))
    ;; (printf "keycode ~s ~s~n" code which-key)
    (if (modifier? code)
        (begin (vector-set! keycodes 0 (+ (vector-ref keycodes 0)
                                          (unmodify code)))
               #f)
        code)))

;; translate key numbers into specific USB keycodes
(define (keycodes-for keys-pressed key-count keycodes)
  (if (= 0 key-count)
      (vector->list keycodes)
      (let ((keycode (keycode-for keys-pressed key-count keycodes)))
        (if keycode
            (vector-set! keycodes key-count keycode)
            #f)
        (keycodes-for keys-pressed (- key-count 1) keycodes))))


;;; showtime

(define (init)
  (for-each-vector output row-pins)
  (for-each-vector high row-pins)
  (for-each-vector input column-pins)
  (for-each-vector high column-pins) ; activate pullup resistors

  (call-c-func "usb_init")
  (pause 200))

(define (usb-send modifiers key1 key2 key3 key4 key5 key6)
  (call-c-func "usb_send" modifiers key1 key2 key3 key4 key5 key6))

(define (loop)
  (free! (let ((keys-pressed (vector 0 0 0 0 0 0 0)))
           ;; scanning the matrix tells us only which physical keys were
           ;; pressed and how many; it doesn't tell us which keycodes to
           ;; send yet.
           (let ((key-count (scan-matrix keys-pressed 1 rows)))
             (apply usb-send (keycodes-for keys-pressed (- key-count 1)
                                           (vector 0 0 0 0 0 0 0))))))
  (loop))

(init)
(loop)
