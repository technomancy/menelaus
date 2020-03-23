#lang racket/gui

(require racket/match)

;; TODO:
;; * enter any arbitrary key by name
;; * save/load layouts
;; * keycode translation

(include "keycodes.scm")

(define width 260)
(define height 132)

(define cols 12)
(define rows 4)
(define angle (degrees->radians 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Drawing

(define alps-switch-width 15.34)
(define alps-switch-height 12.49)
(define cherry-switch-width 13.62)
(define cherry-switch-height 13.72)
(define cherry? false)
(define switch-height (if cherry? cherry-switch-height alps-switch-height))
(define switch-width (if cherry? cherry-switch-width alps-switch-width))

(define switch-spacing 19.0)
(define bottom 95) ; outer bottom

(define column-offsets `(8 5 0 6 11 8 8 11 6 0 5 8))

(define (draw-switch canvas row col)
  (let* ([x (* (+ 1 col) switch-spacing)]
         [y (+ (list-ref column-offsets col) (* switch-spacing (+ row 1)))])
    (send canvas draw-rectangle x y switch-width switch-height)
    (list x y)))

(define hand-height (+ (* switch-spacing rows) (- switch-spacing switch-height)
                       (list-ref column-offsets 0)))
(define switch-x-offset -6.5)
(define switch-y-offset (- bottom hand-height -3.5))

(struct state (layers layer row col mode scale) #:transparent #:mutable)

(define (selected? st row col)
  (and (= row (state-row st)) (= col (state-col st))))

(define (selected st)
  (+ (state-col st) (* (state-row st) cols)))

(define font (make-font #:size 8 #:face "Inconsolata"))
(define small-font (make-font #:size 4 #:face "Inconsolata"))

(define ((draw st) _ canvas)
  (send canvas set-scale (state-scale st) (state-scale st))
  (for/list ([col (in-range cols)]
             #:when true
             [row (if (or (= 5 col) (= 6 col)) '(2 3) (in-range rows))])
    (send canvas set-pen (if (selected? st row col)
                             "red" "black") 1 'solid)
    (if (and (equal? (state-mode st) 'set) (selected? st row col))
        (send canvas set-brush "black" 'solid)
        (send canvas set-brush "black" 'transparent))
    (let* ((xy (draw-switch canvas row col))
           (key (vector-ref (vector-ref (state-layers st)
                                        (state-layer st))
                            (+ col (* row cols))))
           (special? (and key (< 1 (string-length key)))))
      (when key
        (send canvas set-font (if special? small-font font))
        (send canvas draw-text key
              (+ (first xy) (if special? 2 4))
              (+ (second xy) (if special? 2 0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Output

(define prelude
  '((include "keycodes.scm")
    (define rows (list 0 1 2 3))
    (define columns (list 0 1 2 3 4 5 6 7 8 9 10))
    (define row-pins (vector 3 2 1 0))
    (define column-pins (vector 6 5 9 8 7 4 10 19 18 12 11))
    (define layers #f)
    (define current-layer #f)
    (define momentary-layer #f)

    (define (fn on?) (set! momentary-layer (and on? (vector-ref layers 1))))
    (define (set-layer n)
      (lambda (_) (set! current-layer (vector-ref layers n))))))

(define postlude
  '((set! current-layer (vector-ref layers 0))
    (include "menelaus.scm")))

(define (racket-key->ms-key key)
  'key-a)

(define (layers-form layers)
  `((set! layers (vector ,@(for/list ([layer layers])
                             `(vector ,@(for/list ([key layer])
                                          (racket-key->ms-key key))))))))

(define (write-layout filename layers)
  (when (file-exists? filename) (delete-file filename))
  (call-with-output-file filename
    (λ (op)
      (for ([f (append prelude (layers-form layers) postlude)])
        (pretty-print f op 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Updating

(define (move st dx dy)
  (set-state-row! st (modulo (+ dy (state-row st)) rows))
  (set-state-col! st (modulo (+ dx (state-col st)) cols)))

(define (handle-select st keycode)
  (case keycode
    [(right) (move st 1 0)]
    [(left) (move st -1 0)]
    [(up) (move st 0 -1)]
    [(down) (move st 0 1)]
    [(#\-) (set-state-scale! st (* (state-scale st) 0.9))]
    [(#\=) (set-state-scale! st (* (state-scale st) 1.1))]
    [(escape) (set-state-mode! st 'quit)]
    [(#\space) (set-state-mode! st 'set)]
    [(#\tab) (printf "~s~n" st) st]
    [(#\return) (write-layout "out.scm" (state-layers st))]
    [(release) #f]
    [else (printf "~s~n" keycode) st]))

(define (key-for keycode)
  (case keycode
    [(control) "ctrl"]
    ;; TODO: alt and super for some reason don't show at all??
    [(escape) "esc"]
    [(shift) "shft"]
    [(insert) "ins"]
    [(next) "pgdn"]
    [(prior) "pgup"]
    [(#\rubout) "del"]
    [(#\space) "spc"]
    [(#\backspace) "bksp"]
    [(#\return) "enter"]
    [else (format "~a" keycode)]))

(define (handle-set st keycode)
  (unless (equal? 'release keycode)
    (set-state-mode! st 'select)
    (vector-set! (vector-ref (state-layers st) (state-layer st))
                 (selected st) (key-for keycode))))

(define (handle-key canvas st keycode)
  (case (state-mode st)
    ['select (handle-select st keycode)]
    ['set (handle-set st keycode)])
  (send canvas refresh))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main

(define (main)
  (let ([frame (new frame% [label "Menelaus Keyboard Layout Editor"])]
        [st (state (vector (make-vector (* rows cols) #f)
                           (make-vector (* rows cols) #f)) 0 0 0 'select 2.5)])
    (new (class canvas%
           (define/override (on-char event)
             (handle-key this st (send event get-key-code))
             (when (equal? 'quit (state-mode st))
               (send frame show #f)))
           (super-new))
         [parent frame]
         [paint-callback (draw st)])
    (send frame show #t)))

(module+ main
  (main))
