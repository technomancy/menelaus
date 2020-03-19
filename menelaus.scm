(include "keycodes.scm")

(define rows (list 0 1 2 3))
(define row-pins (vector 3 2 1 0))
(define columns (list 0 1 2 3 4 5 6 7 8 9 10))
(define column-pins (vector 6 5 9 8 7 4 10 19 18 12 11))

(define max-keys 10) ; single USB frame can only send 6 keycodes plus modifiers

;; pcbdown flip, comment out for normal
;; (begin (set! mod-alt (modify 1))
;;        (set! mod-ctrl (modify 3))
;;        (set! column-pins (vector 11 12 18 19 10 4 7 8 9 5 6)))

(include "layout.scm")

;;;;;;;;;;;;;;;;;;; utils

(define (find-aux v x n max)
  (let ((y (vector-ref v n)))
    (if (and y (= x y))
        n
        (and (< n max)
             (find-aux v x (+ n 1) max)))))

(define (find v x)
  (find-aux v x 0 (- (vector-length v) 1)))

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
  (if (null? columns-left)
      scan
      (scan-column (scan-key scan row (car columns-left))
                   row (cdr columns-left))))

(define (activate-row row)
  (for-each-vector high row-pins)
  (low (vector-ref row-pins row)))

(define (scan-matrix scan rows-left)
  (if (null? rows-left)
      scan
      (begin
        (activate-row (car rows-left))
        (scan-matrix (scan-column scan (car rows-left) columns)
                     (cdr rows-left)))))

;;;;;;;;;;;;;;;;;;; debouncing

(define debounce-passes 3)

(define (debounce-matrix-aux last-scan passes-left)
  (if (< 0 passes-left)
      (let ((this-scan (scan-matrix (list) rows)))
        (if (equal? this-scan last-scan)
            (debounce-matrix-aux this-scan (- passes-left 1))
            (debounce-matrix-aux this-scan debounce-passes)))
      last-scan))

(define (debounce-matrix)
  (debounce-matrix-aux (list) debounce-passes))

;;;;;;;;;;;;;;;;;;; press and release tracking

(define last-keys-down (vector #f #f #f #f #f #f #f #f #f #f))

(define (add-last-down-aux key n)
  (if (not (vector-ref last-keys-down n))
      (vector-set! last-keys-down n key)
      (if (< n 9)
          (add-last-down-aux key (+ n 1))
          ;; microscheme does not have a `when' form, so for compatibility with
          ;; racket, we must always include an else branch.
          #f)))

(define (remove-last-down-aux key n)
  (if (equal? key (vector-ref last-keys-down n))
      (vector-set! last-keys-down n #f)
      (and (< n 9) (remove-last-down-aux key (+ n 1)))))

(define (add-last-down key) (add-last-down-aux key 0))
(define (remove-last-down key) (remove-last-down-aux key 0))

(define (remove-aux v lst checked all?)
  ;; also missing the cond form
  (if (null? lst)
      (reverse checked)
      (if (equal? v (car lst))
          (if all?
              (remove-aux v (cdr lst) checked all?)
              (reverse (append (cdr lst) checked)))
          (remove-aux v (cdr lst) (cons (car lst) checked) all?))))

(define (remove v lst) (remove-aux v lst (list) #f))
(define (remove-all v lst) (remove-aux v lst (list) #t))

(define (press/release-aux press release keys-scanned)
  (if (null? keys-scanned)
      (cons press release)
      (let ((key (car keys-scanned)))
        (if (member key release)
            (press/release-aux press (remove key release) (cdr keys-scanned))
            (press/release-aux (cons key press) release (cdr keys-scanned))))))

(define (press/release-for keys-scanned)
  (let ((p/r (press/release-aux (list)
                                (remove-all #f (vector->list last-keys-down))
                                keys-scanned)))
    ;; save off press/release into last-keys-down for next cycle
    (for-each add-last-down (car p/r))
    (for-each remove-last-down (cdr p/r))
    p/r))

;;;;;;;;;;;;;;;;;;; using press/release data to generate keycodes

(define (lookup key-pos)
  (let ((layout (or momentary-layer current-layer)))
    (vector-ref layout key-pos)))

(define modifiers (vector 0 0 0 0))
(define keycodes-down (vector 0 0 0 0 0 0))

;; which keys caused the keycodes/modifiers to be down?
(define keys-for-modifiers (vector #f #f #f #f))
(define keys-for-frame (vector #f #f #f #f #f #f))

(define (press-modifier keycode key)
  (vector-set! modifiers (- keycode 1) 1)
  (vector-set! keys-for-modifiers (- keycode 1) key))

(define (release-modifier keycode key n)
  (if (= (or (vector-ref keys-for-modifiers n) (- 0 1)) key)
      (begin
        (vector-set! modifiers n 0)
        (vector-set! keys-for-modifiers n #f))
      (if (< n 3)
          (release-modifier keycode key (+ n 1))
          #f)))

(define (press-normal-key keycode key)
  (let ((slot (find keycodes-down 0)))
    (and slot (vector-set! keycodes-down slot keycode))
    (and slot (vector-set! keys-for-frame slot key))))

(define (press-key key)
  (let ((keycode (lookup key)))
    (if (procedure? keycode)
        (keycode #t)
        (if (modifier? keycode)
            (begin (press-modifier (unmodify keycode) key)
                   (if (uncombo keycode)
                       (press-normal-key (uncombo keycode) key)
                       #f))
            (press-normal-key keycode key)))))

(define (release-key key)
  ;; lookup here looks it up in the current layer, even if it was pressed in
  ;; the momentary layer. these need to be consistent across layers or tracked
  ;; in a similar manner as keys-for-frame.
  (let ((keycode (lookup key)))
    (if (procedure? keycode)
        (keycode #f)
        (let ((slot (find keys-for-frame key))
              (modifier-slot (find keys-for-modifiers key)))
          (if slot
              (begin
                (vector-set! keycodes-down slot 0)
                (vector-set! keys-for-frame slot #f))
              #f)
          (if modifier-slot
              (release-modifier modifier-slot key 0)
              #f)))))

;;;;;;;;;;;;;;;;;;; showtime

(define (set-usb-frame press/release)
  (let ((press (car press/release))
        (release (cdr press/release)))
    (for-each press-key press)
    (for-each release-key release)
    keycodes-down))

(define (init)
  (set! current-layer (vector-ref layers 0))
  (for-each-vector output row-pins)
  (for-each-vector high row-pins)
  (for-each-vector input column-pins)
  (for-each-vector high column-pins) ; activate pullup resistors

  (call-c-func "usb_init")
  (pause 200))

(define (usb-send m k0 k1 k2 k3 k4 k5)
  ;; call-c-func is a special form and cannot be applied
  (let ((mods (+ (vector-ref m 0) (* (vector-ref m 1) 2)))) ; + isn't variadic
    (let ((mods (+ mods (+ (* (vector-ref m 2) 4) (* (vector-ref m 3) 8)))))
      (call-c-func "usb_send" mods k0 k1 k2 k3 k4 k5))))

(define (loop)
  ;; scanning the matrix tells us only which physical keys were pressed and
  ;; how many; it doesn't tell us which keycodes to send yet.
  (free! (let ((keys-scanned (debounce-matrix)))
           (set-usb-frame (press/release-for keys-scanned))
           (apply usb-send (cons modifiers (vector->list keycodes-down)))))
  (loop))

(init)
(loop)
