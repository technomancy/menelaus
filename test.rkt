#lang racket

;; this file simulates the hardware necessary to test the keyboard firmware,
;; because doing actual development on an atmega32u4 is nightmarishly tedious.

(define pins (make-vector 20))
(define keys (make-vector 44 #f))

(define output void) ; don't bother to simulate pin modes
(define input void)
(define pause void)

(define (high pin) (vector-set! pins pin #t))
(define (low pin) (vector-set! pins pin #f))

;; microscheme has this as a separate form
(define for-each-vector vector-map)

(define last-usb-frame #f) ; save this off so we can test it

(define (mods-list mods)
  (filter symbol? (list (if (positive? (bitwise-and mods 1)) 'ctrl 0)
                        (if (positive? (bitwise-and mods 2)) 'shift 0)
                        (if (positive? (bitwise-and mods 4)) 'alt 0)
                        (if (positive? (bitwise-and mods 8)) 'super 0))))

(define (usb-save mods . args)
  (set! last-usb-frame (cons (mods-list mods) args)))

(define (call-c-func f-name . args)
  (when (equal? f-name "usb_send")
    (apply usb-save args)))

(define (active-row)
  ;; hypothetically we could have multiple active rows but we just assume one
  (for/first ([pin row-pins]
              [row (range (length rows))]
              #:when (not (vector-ref pins pin)))
    row))

(define (col-for pin)
  (for/first ([c-pin column-pins]
              [col (range (length columns))]
              #:when (= c-pin pin))
    col))

(define (low? pin)
  ;; (when (vector-ref keys (offset-for (active-row) (col-for pin)))
  ;;     (printf "lookup ~s ~s ~n" pin (offset-for (active-row) (col-for pin)))
  ;;     (printf "Keys ~s~n" keys))
  (vector-ref keys (offset-for (active-row) (col-for pin))))

(define (make-test-data)
  ;; have to put this in a function so we can internal-define; eww
  (include "keycodes.scm")
  ;; each test case is a pair of inputs->outputs
  ;; inputs are a list of keys (by offset), outputs are elements of a USB frame
  `(;; single key
    ((3) . (() ,key-r))
    ;; another single key
    ((2) . (() ,key-e))
    ;; the first key in the whole layout
    ((0) . (() ,key-q))
    ;; multiple normal keys
    ((2 3) . (() ,key-e ,key-r))
    ;; modifier keys (ctrl)
    ((27) . ((ctrl)))
    ;; two modifiers (shift+ctrl) get ORed together
    ((27 36) . ((ctrl shift)))
    ;; modifier (shift) and normal key
    ((36 4) . ((shift) ,key-t))
    ;; modifier and multiple normal keys
    ((36 4 6) . ((shift) ,key-t ,key-y))
    ;; fn key alone
    ((40) . (()))
    ;; fn key and normal key
    ((40 1) . ((shift) ,key-2))
    ;; fn key and modifier and normal key
    ((40 35 2) . ((super) ,key-up))
    ;; releasing fn should leave the previously-pressed key on the fn layer!!!
    ((2) . (() ,key-up))

    ;; fn key alone
    ((40) . (()))
    ;; fn key and *
    ((40 28) . ((shift) ,key-8))
    ;; fn is released
    ((28) . ((shift) ,key-8))
    ;; * is released
    (() . (()))
    ;; normal key doesn't leave shift down
    ((0) . (() ,key-q))

    ;; changing to L2 (fn+esc)
    ((40) . (()))
    ((40 33) . (()))
    ;; fn+esc should stay on L2 across multiple scans
    ((40 33) . (()))
    ;; release fn to disable momentary
    (() . (()))
    ;; hitting an L2 key
    ((1) . (() ,key-home))
    ;; L2 two keys and mod
    ((36 39 18) . ((shift) ,key-f4 ,key-space))
    ;; back to base (fn)
    ((40) . (()))
    ;; base layer key
    ((2) . (() ,key-e))
    ;; seven keys down
    ((1 2 3 4 7 8 9) . (() ,key-e ,key-w ,key-r ,key-t ,key-u ,key-i))
    ;; shift combo and shift key simultaneously
    ((40) . (()))
    ((40 1 36) . ((shift) ,key-2))
    ((40 1) . (() ,key-2))
    ((40) . (()))
    (() . (()))))

(define test-data (make-test-data))

(define failures '())

(define (fail expected actual)
  (printf "F")
  (set! failures
        (cons (format "Expected ~s, got ~s~n" expected actual) failures)))

(define (finish)
  (printf (string-join (reverse failures)
                       "~n" #:before-first "~n" #:after-last "~n"))
  (exit (if (empty? failures) 0 1)))

;; we can perform our checks here and make changes to the pin state.
(define-syntax free!
  (syntax-rules ()
    [(free! body) (if (empty? test-data)
                      (finish)
                      (let ([test-case (car test-data)])
                        (for ([i (vector-length keys)])
                          (vector-set! keys i
                                       (and (member i (car test-case)) #t)))
                        body
                        (let ((actual (cons (car last-usb-frame)
                                            (remove-all
                                             0 (cdr last-usb-frame)))))
                          (if (equal? (cdr test-case) actual)
                              (printf ".")
                              (fail (cdr test-case) actual)))
                        (set! test-data (cdr test-data))))]))

(include "menelaus.scm")
