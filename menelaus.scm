;; -*- scheme -*-

;; menelaus.scm

(include "keyboard.scm")

(define pressed 0)

(define column-pins (list 11 12 15 14 10 4 7 8 9 5 6))
(define layout (vector key-a key-s key-d key-f key-g
                       key-h key-j key-k key-l key-semicolon))

(define (scan-column pin keycode)
  (if (and (= 0 pressed) (low? pin))
      (set! pressed keycode)))

(define (loop)
  (set! pressed 0)
  ;; this causes a vector exception
  ;; (for 0 10 (lambda (n) (scan-column (vector-ref column-pins n)
  ;;                                    (vector-ref layout n))))
  ;; while the unrolled version works great
  (scan-column 11 key-a)
  (scan-column 12 key-s)
  (scan-column 15 key-d)
  (scan-column 14 key-f)
  (scan-column 10 key-g)

  (scan-column 7 key-h)
  (scan-column 8 key-j)
  (scan-column 9 key-k)
  (scan-column 5 key-l)
  (scan-column 6 key-semicolon)
  (call-c-func "usb_send" 0 pressed 0 0 0 0 0)
  (loop))

(define (init)
  ;; leonardo pins only go up to 13, but a-star goes to 19
  (set! arduino-ports (vector #x29 #x29 #x29 #x29 #x29 #x26 #x29 #x2C
                              #x23 #x23 #x23 #x23 #x29 #x26
                              #x23 #x23 #x23 #x29 #x2D #x2D))
  (set! arduino-pins (vector 4 8 2 1 16 64 128 64 16 32 64 128 64 128
                             8 2 4 32 65 128))
  ;; this causes a vector exception:
  ;; (for-each-vector column-pins input)
  ;; (for-each-vector column-pins high) ; activate pullup resistors
  ;; while unrolling the loop works fine
  (high 11)
  (high 12)
  (high 15)
  (high 14)
  (high 10)
  (high 4)
  (high 7)
  (high 8)
  (high 9)
  (high 5)
  (high 6)
  (call-c-func "usb_init")
  (pause 200))

(init)
(loop)
