#lang racket/gui

(require racket/match)

(define (vector-set v i o) ; =(
  (vector->immutable-vector
   (for/vector ([j (in-range (vector-length v))])
     (if (= i j)
         o
         (vector-ref v j)))))

(define call-c-func void) ; for microscheme compatibility

(include "keycodes.scm")
(include "layout.scm")

(define width 260)
(define height 132)

(define cols 12)
(define rows 4)
(define angle (degrees->radians 10))

(define alps-switch-width 15.34)
(define alps-switch-height 12.49)
(define cherry-switch-width 13.62)
(define cherry-switch-height 13.72)
(define cherry? false)
(define switch-height (if cherry? cherry-switch-height alps-switch-height))
(define switch-width (if cherry? cherry-switch-width alps-switch-width))

(define switch-spacing 19.0)
(define bottom 95) ; outer bottom

(define column-offsets `(8 5 0 6 11
                           8
                           8
                           11 6 0 5 8))

(define (draw-switch canvas row col)
  (let* ([x (* (+ 1 col) switch-spacing)]
         [y (+ (list-ref column-offsets col) (* switch-spacing (+ row 1)))])
    (send canvas draw-rectangle x y switch-width switch-height)
    (list x y)))

(define hand-height (+ (* switch-spacing rows) (- switch-spacing switch-height)
                       (list-ref column-offsets 0)))
(define switch-x-offset -6.5)
(define switch-y-offset (- bottom hand-height -3.5))

(struct state (layers layer row col mode scale) #:transparent)

(define (selected? st row col)
  (and (= row (state-row st)) (= col (state-col st))))

(define (selected st)
  (+ (state-col st) (* (state-row st) cols)))

(define font (make-font #:size 8 #:face "Inconsolata"))
(define small-font (make-font #:size 4 #:face "Inconsolata"))

(define ((draw state-box) _ canvas)
  (let ((st (unbox state-box)))
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
                (+ (second xy) (if special? 2 0))))))))

(define (move st dx dy)
  (struct-copy state st
               (row (modulo (+ dy (state-row st)) rows))
               (col (modulo (+ dx (state-col st)) cols))))

(define (handle-select st keycode)
  (case keycode
    [(right) (move st 1 0)]
    [(left) (move st -1 0)]
    [(up) (move st 0 -1)]
    [(down) (move st 0 1)]
    [(#\-) (struct-copy state st (scale (* (state-scale st) 0.9)))]
    [(#\=) (struct-copy state st (scale (* (state-scale st) 1.1)))]
    ['escape (struct-copy state st (mode 'quit))]
    [(#\return) (struct-copy state st (mode 'set))]
    [(#\tab) (printf "~s~n" st) st]
    [(release) st]
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

(define (update-layout st keycode)
  (vector-set (state-layers st) (state-layer st)
              (vector-set (vector-ref (state-layers st)
                                      (state-layer st))
                          (selected st) (key-for keycode))))

(define (handle-set st keycode)
  (if (equal? 'release keycode)
      st
      (struct-copy state st
                   (layers (update-layout st keycode))
                   (mode 'select))))

(define (handle-key canvas state-box keycode)
  (let ((st (unbox state-box)))
    (case (state-mode st)
      ['select (set-box! state-box (handle-select st keycode))]
      ['set (set-box! state-box (handle-set st keycode))])
    (send canvas refresh)))

(define (main)
  (let ([frame (new frame% [label "Menelaus"])]
        [state-box (box (state (vector (make-vector (* rows cols) #f))
                               0 0 0 'select 2.5))])
    (new (class canvas%
           (define/override (on-char event)
             (handle-key this state-box (send event get-key-code))
             (when (equal? 'quit (state-mode (unbox state-box)))
               (send frame show #f)))
           (super-new))
         [parent frame]
         [paint-callback (draw state-box)])
    (send frame show #t)))

(module+ main
  (main))
