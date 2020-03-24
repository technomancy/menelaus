#lang racket/gui

(require racket/match)

;; TODO:
;; * add/remove layers

(include "keycodes.scm")

(define width 260)
(define height 132)

(define cols 12)
(define rows 4)
(define angle (degrees->radians 10))

(struct state (layers layer row col mode scale) #:transparent #:mutable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Drawing

(define switch-width 15.34)
(define switch-height 12.49)
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

(define (selected? st row col)
  (and (= row (state-row st)) (= col (state-col st))))

(define (selected st)
  (+ (state-col st) (* (state-row st) cols)))

(define font (make-font #:size 8 #:face "Inconsolata"))
(define small-font (make-font #:size 4 #:face "Inconsolata"))

(define (layer-text st)
  (format "Layer ~s/~s" (state-layer st)
          (sub1 (vector-length (state-layers st)))))

(define (draw st canvas)
  (send canvas set-scale (state-scale st) (state-scale st))
  (for/list ([col (in-range cols)]
             #:when true
             [row (if (or (= 5 col) (= 6 col)) '(2 3) (in-range rows))])
    (send canvas set-pen (if (selected? st row col)
                             "red" "black") 1 'solid)
    (cond [(and (equal? (state-mode st) 'set) (selected? st row col))
           (send canvas set-brush "black" 'solid)]
          [(and (equal? (state-mode st) 'set-shifted) (selected? st row col))
           (send canvas set-brush "black" 'cross-hatch)]
          ['else (send canvas set-brush "black" 'transparent)])
    (let* ((xy (draw-switch canvas row col))
           (key (vector-ref (vector-ref (state-layers st)
                                        (state-layer st))
                            (+ col (* row cols))))
           (special? (and key (< 1 (string-length key)))))
      (when key
        (send canvas set-font (if special? small-font font))
        (send canvas draw-text key
              (+ (first xy) (if special? 2 4))
              (+ (second xy) (if special? 2 0))))))
  (send canvas set-font small-font)
  (send canvas draw-text (layer-text st) 180 108)
  (for ([msg '("Arrows: select key" "Space: set keycode"
                                    "Shift: set shifted keycode"
                                    "Tab: set special keycode"
                                    "[ and ]: change layer"
                                    "Enter: save layout"
                                    "L: load layout")]
        [i (in-range 5)])
    (send canvas draw-text msg 15 (+ 108 (* i 8)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Scheme Output

(define some-shifts
  #hash((#\1 . #\!) (#\2 . #\@) (#\3 . #\#) (#\4 . #\$) (#\5 . #\%)
        (#\6 . #\^) (#\7 . #\&) (#\8 . #\*) (#\9 . #\() (#\0 . #\))
        (#\= . #\+) (#\' . #\") (#\, . #\<) (#\. . #\>) (#\/ . #\?)
        (#\; . #\:) (#\[ . #\{) (#\] . #\}) (#\\ . #\|) (#\- . #\_)
        (#\` . #\~)))

;; Add in shifted ASCII letters programmatically
(define shifts (for/fold ([all some-shifts])
                         ([n (in-range 97 123)])
                 (hash-set all (integer->char n) (integer->char (- n 32)))))

(define (shift keycode) (hash-ref shifts keycode keycode))
(define (unshift keycode) (for/first ([(k v) shifts] #:when (eq? v keycode)) k))

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

;; These are the exceptions to the symbol->keycode translation rules:
(define special-keycodes #hash(("ctrl" . mod-ctrl)
                               ("alt" . mod-alt)
                               ("shft" . mod-shift)
                               ("super" . mod-super)
                               (";" . key-semicolon)
                               ("`" . key-backtick)
                               ("," . key-comma)
                               ("'" . key-quote)
                               ("\\" . key-backslash)
                               ("[" . key-left-bracket)
                               ("]" . key-right-bracket)
                               ("fn" . fn)))

;; L1, L2, L3, etc are treated as layer-switching functions.
(define (layer-switching-keycode key)
  (and (string? key) (regexp-match #rx"^L[0-9]+$" key)
       `(set-layer ,(string->number (substring key 1)))))

;; Convert a shifted character into a (sft key-N) form microscheme expects.
(define (shifted-keycode key convert)
  (and (unshift key)
       (let* ([char (first (string->list (symbol->string key)))]
              [new-char (unshift char)]
              [sym (string->symbol (list->string (list new-char)))])
         `(sft ,(convert sym)))))

;; Convert keys from our label to microscheme's representation.
(define (racket-key->ms-key key)
  (let ((sym (string->symbol (format "key-~a" key))))
    (or (hash-ref special-keycodes key #f)
        (layer-switching-keycode key)
        (shifted-keycode key racket-key->ms-key)
        (with-handlers ([exn? (λ (_) 0)])
          ;; Try to see if the key is defined in keycodes.scm
          ;; TODO: find a way to do this without eval
          (and (eval sym) sym)))))

(define (fix-row row mid)
  (append (take row 5) (list mid) (take (drop row 7) 5)))

;; In the GUI, we have 12 columns, the middle two of which are half-columns;
;; in Microscheme we have 11 columns; the 4 middle keys are all in the middle
;; column. This function converts a 4x12 grid into an 11-column vector.
(define (fix-columns layer)
  (let ([layer (vector->list layer)])
    (append (fix-row layer (list-ref layer 29))
            (fix-row (drop layer 12) (list-ref layer 30))
            (fix-row (drop layer 24) (list-ref layer 41))
            (fix-row (drop layer 36) (list-ref layer 42)))))

(define (layers-form layers)
  `((set! layers (vector ,@(for/list ([layer layers])
                             `(vector ,@(for/list ([key (fix-columns layer)])
                                          (racket-key->ms-key key))))))))

(define (write-layout filename st)
  (when (file-exists? filename) (delete-file filename))
  (call-with-output-file filename
    (λ (out)
      (display ";; " out)
      (write st out)
      (display "\n;; This file was generated by the Menelaus GUI.\n\n" out)
      (for ([f (append prelude (layers-form (state-layers st)) postlude)])
        (pretty-print f out 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Save/load

(define (load-state reset)
  (let ([filename (get-file "Load layout:")])
    (when filename
      (call-with-input-file filename
        (lambda (in)
          (read-bytes 2 in) ; skip initial comment
          ;; reading it back in gives us a vector starting with 'struct:state
          ;; instead of an actual state struct for some reason, so we convert
          ;; to a list, drop the car, and call the state constructor.
          (reset (apply state (cdr (vector->list (read in))))))))))

(define (save-state st)
  (let ([filename (put-file "Save to:")])
    (when filename
      (write-layout filename st)
      (let ([dia (new dialog% [label "Layout saved"])])
        (new message%
             [label (format "Layout saved to ~a."
                            (path->string filename))]
             [parent dia])
        (new button%
             [label "OK"]
             [parent dia]
             [callback (lambda _ (send dia show #f))])
        (send dia show #t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Handlers

(define (key-for keycode)
  (case keycode
    [(control) "ctrl"]
    ;; TODO: alt and super for some reason don't show at all??
    ;; for now they're handled as specials
    [(escape) "esc"]
    [(shift) "shft"]
    [(insert) "ins"]
    [(next) "pgdn"]
    [(prior) "pgup"]
    [(#\rubout) "del"]
    [(#\space) "spc"]
    [(#\tab) "tab"]
    [(#\backspace) "bksp"]
    [(#\return) "enter"]
    [(#f) #f]
    [else (format "~a" keycode)]))

(define (handle-set st keycode shifted?)
  (unless (equal? 'release keycode)
    (set-state-mode! st 'select)
    (vector-set! (vector-ref (state-layers st) (state-layer st))
                 (selected st) (if shifted?
                                   (key-for (shift keycode))
                                   (key-for keycode)))))

;; Some keys can't be represented with a single keypress, such as fn or L2.
(define (set-special! st)
  (let* ([dia (new dialog% [label "Select special key"])]
         [choice (new choice%
                      [label "Special key:"]
                      [parent dia]
                      [choices '["fn" "L2" "super" "alt"]])])
    (new button%
         [label "OK"]
         [parent dia]
         [callback (lambda _
                     (handle-set st (send choice get-string-selection) false)
                     (send dia show #f))])
    (send dia show #t)))

(define (non-existent-key? row col)
  (and (< row 2) (or (= col 5) (= col 6))))

(define (move st dx dy)
  (set-state-row! st (modulo (+ dy (state-row st)) rows))
  (set-state-col! st (modulo (+ dx (state-col st)) cols))
  (when (non-existent-key? (state-row st) (state-col st))
    (move st dx dy)))

(define (change-layer st dir)
  (set-state-layer! st (modulo (+ dir (state-layer st))
                               (vector-length (state-layers st)))))

(define (handle-select st keycode reset)
  (case keycode
    [(right) (move st 1 0)]
    [(left) (move st -1 0)]
    [(up) (move st 0 -1)]
    [(down) (move st 0 1)]
    [(#\-) (set-state-scale! st (* (state-scale st) 0.9))]
    [(#\=) (set-state-scale! st (* (state-scale st) 1.1))]
    [(#\[) (change-layer st 1)]
    [(#\]) (change-layer st 1)]
    [(#\backspace) (handle-set st false)]
    [(escape) (set-state-mode! st 'quit)]
    [(#\space) (set-state-mode! st 'set)]
    [(shift) (set-state-mode! st 'set-shifted)]
    [(#\`) (printf "~a~n" st)]
    [(#\tab) (set-special! st)]
    [(#\return) (save-state st)]
    [(#\l) (load-state reset)]
    [(release) #f]
    [else (printf "~s~n" keycode) st]))

(define (handle-key canvas st keycode reset)
  (case (state-mode st)
    ['select (handle-select st keycode reset)]
    ['set (handle-set st keycode false)]
    ['set-shifted (handle-set st keycode true)])
  (send canvas refresh))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main

(define (main)
  (let ([frame (new frame% [label "Menelaus Keyboard Layout Editor"])]
        [st (state (vector (make-vector (* rows cols) #f)
                           (make-vector (* rows cols) #f)
                           (make-vector (* rows cols) #f)) 0 0 0 'select 2.5)])
    (new (class canvas%
           (define/override (on-char event)
             (handle-key this st (send event get-key-code)
                         (lambda (new-st) (set! st new-st) (send this refresh)))
             (when (equal? 'quit (state-mode st))
               (send frame show #f)))
           (super-new))
         [parent frame]
         [paint-callback (lambda (_ canvas)
                           (draw st canvas))])
    (send frame show #t)))

(module+ main
  (main))
