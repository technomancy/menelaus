(include "keycodes.scm")
(include "layout.scm")

(define rows (list 0 1 2 3))
(define row-pins (vector 3 2 1 0))
(define columns (list 0 1 2 3 4 5 6 7 8 9 10))
(define column-pins (vector 6 5 9 8 7 4 10 19 18 12 11))

(define max-keys 10) ; single USB frame can only send 6 keycodes plus modifiers

;;;;;;;;;;;;;;;;;;; matrix

(define (offset-for row col)
  (+ col (* row (length columns))))

(define (scan-key scan row col)
  (if (and (< (length scan) max-keys)
           ;; pullup resistors mean a closed circuit is low rather than high
           (low? (vector-ref column-pins col)))
      (cons (offset-for row col) scan)
      scan))

(define (scan-column scan row columns-left)
  (if (empty? columns-left)
      scan
      (scan-column (scan-key scan row (car columns-left))
                   row (cdr columns-left))))

(define (activate-row row)
  (for-each-vector high row-pins)
  (low (vector-ref row-pins row)))

(define (scan-matrix scan rows-left)
  (if (empty? rows-left)
      scan
      (begin
        (activate-row (car rows-left))
        (scan-matrix (scan-column scan (car rows-left) columns)
                     (cdr rows-left)))))

;;;;;;;;;;;;;;;;;;; debouncing

(define debounce-passes 4)

(define (debounce-matrix-aux last-scan passes-left)
  (if (< 0 passes-left)
      (let ((this-scan (scan-matrix (list) rows)))
        (if (equal? this-scan last-scan)
            (debounce-matrix-aux this-scan (- passes-left 1))
            (debounce-matrix-aux this-scan debounce-passes)))
      last-scan))

(define (debounce-matrix)
  (debounce-matrix-aux (list) debounce-passes))

;;;;;;;;;;;;;;;;;;; layout

(define (lookup key-pos)
  (let ((layout (or momentary-layer current-layer)))
    (vector-ref layout key-pos)))

(define (keycode-for key-pos keycodes)
  (let ((code (lookup key-pos)))
    ;; (printf "keycode ~s ~s~n" code which-key)
    (if (modifier? code)
        (begin (vector-set! keycodes 0 (+ (vector-ref keycodes 0)
                                          (unmodify code)))
               (uncombo code))
        (and (not (procedure? code)) code))))

(define (call-functions keys-scanned)
  (if (empty? keys-scanned)
      #f
      (let ((code (lookup (car keys-scanned))))
        (and (procedure? code) (code))
        (call-functions (cdr keys-scanned)))))

(define (first-zero v n)
  (if (or (= 0 (vector-ref v n)) (= 6 n))
      n
      (first-zero v (+ n 1))))

;; translate key numbers into specific USB keycodes
(define (keycodes-for keys-scanned keycodes)
  ;; this happens before we look up "regular" keycodes because it changes layers
  (call-functions keys-scanned)
  (if (empty? keys-scanned)
      (vector->list keycodes)
      (let ((keycode (keycode-for (car keys-scanned) keycodes)))
        (and keycode
             (vector-set! keycodes (first-zero keycodes 1) keycode))
        (keycodes-for (cdr keys-scanned) keycodes))))

;;;;;;;;;;;;;;;;;;; showtime

(define (init)
  (set! current-layer (vector-ref layers 0))
  (for-each-vector output row-pins)
  (for-each-vector high row-pins)
  (for-each-vector input column-pins)
  (for-each-vector high column-pins) ; activate pullup resistors

  (call-c-func "usb_init")
  (pause 200))

(define (usb-send modifiers key1 key2 key3 key4 key5 key6)
  (call-c-func "usb_send" modifiers key1 key2 key3 key4 key5 key6))

(define (loop)
  (set! momentary-layer #f)
  ;; scanning the matrix tells us only which physical keys were pressed and
  ;; how many; it doesn't tell us which keycodes to send yet.
  (free! (let ((keys-scanned (debounce-matrix)))
           (apply usb-send (keycodes-for keys-scanned (vector 0 0 0 0 0 0 0)))))
  (loop))

(init)
(loop)
