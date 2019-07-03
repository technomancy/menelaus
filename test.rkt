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

(define (call-c-func f-name . args)
  ;; (printf "FFI ~s~n" args)
  (set! last-usb-frame args))

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
    ((3) . (0 ,key-r 0 0 0 0 0))
    ;; another single key
    ((2) . (0 ,key-e 0 0 0 0 0))
    ;; multiple normal keys
    ((2 3) . (0 ,key-e ,key-r 0 0 0 0))
    ;; modifier keys (alt)
    ((27) . (4 0 0 0 0 0 0))
    ;; two modifiers (shift+alt) get ORed together
    ((27 36) . (6 0 0 0 0 0 0))
    ;; modifier (shift) and normal key
    ((36 4) . (2 ,key-t 0 0 0 0 0))
    ;; modifier and multiple normal keys
    ((36 4 6) . (2 ,key-t ,key-y 0 0 0 0))
    ;; fn key alone
    ((40) . (0 0 0 0 0 0 0))
    ;; fn key and normal key
    ((40 1) . (2 ,key-2 0 0 0 0 0))
    ;; fn key and modifier and normal key
    ((40 35 2) . (8 ,key-up 0 0 0 0 0))
    ;; releasing fn should leave the previously-pressed key on the fn layer!!!
    ;; ((2) . (0 ,key-up 0 0 0 0 0))
    ))

(define test-data (make-test-data))

(define failures '())

(define (fail expected actual)
  (printf "F")
  (set! failures
        (cons (format "Expected ~s, got ~s~n" expected actual) failures)))

(define (finish)
  (printf (string-join failures "~n" #:before-first "~n" #:after-last "~n"))
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
                        (if (equal? (cdr test-case) last-usb-frame)
                            (printf ".")
                            (fail (cdr test-case) last-usb-frame))
                        (set! test-data (cdr test-data))))]))

(include "menelaus.scm")
