#lang racket

(define pins (make-vector 20))
(define keys (make-vector 44 #f))

(define output void)
(define input void)
(define pause void)

(define (high pin) (vector-set! pins pin #t))
(define (low pin) (vector-set! pins pin #f))

(define (for-each-vector f v) (for ([x v]) (f x)))

(define last-usb-frame #f)

(define (call-c-func f-name . args)
  ;; (printf "FFI ~s~n" args)
  (set! last-usb-frame args))

(define (active-row)
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
  ;; pair of pins/keycodes
  `(((1) . (0 ,key-w 0 0 0 0 0))
    ((2) . (0 ,key-e 0 0 0 0 0))))

(define test-data (make-test-data))

(define failures 0)

(define (fail expected actual)
  (set! failures (add1 failures))
  (printf "Expected ~s, got ~s~n" expected actual))

;; we can perform our checks here and make changes to the pin state.
(define-syntax free!
  (syntax-rules ()
    [(free! body) (if (empty? test-data)
                      (exit (if (= 0 failures) 0 1))
                      (let ([test-case (car test-data)])
                        (for ([i (vector-length keys)])
                          (vector-set! keys i
                                       (and (member i (car test-case)) #t)))
                        body
                        (if (equal? (cdr test-case) last-usb-frame)
                            (printf ".~n")
                            (fail (cdr test-case) last-usb-frame))
                        (set! test-data (cdr test-data))))]))

(include "menelaus.scm")
