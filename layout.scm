;;; this is the multidvorak layout

;; we have to declare this up front and set it later because of circularity
(define layers (vector #f #f #f))
(define current-layer #f)
(define momentary-layer #f)

(define (fn)
  (set! momentary-layer (vector-ref layers 1)))

(define (set-layer-2)
  (set! current-layer (vector-ref layers 2)))

(define (set-layer-0)
  (set! current-layer (vector-ref layers 0)))

(vector-set!
 layers 0
 (vector key-q key-w key-e key-r key-t key-backslash
         key-y key-u key-i key-o key-p

         key-a key-s key-d key-f key-g key-backtick
         key-h key-j key-k key-l key-semicolon

         key-z key-x key-c key-v key-b mod-ctrl
         key-n key-m key-comma key-period key-slash

         key-esc key-tab mod-super mod-shift key-backspace mod-alt
         key-space fn key-quote key-left-bracket key-enter))

(vector-set!
 layers 1
 (vector (combo mod-shift key-1) (combo mod-shift key-2) key-up
         (combo mod-shift key-dash) (combo mod-shift key-equal)
         0 key-page-up key-7 key-8 key-9 (combo mod-shift key-8)

         (combo mod-shift key-3) key-left key-down key-right
         (combo mod-shift key-4) 0
         key-page-down key-4 key-5 key-6 (combo mod-shift key-right-bracket)

         key-dash key-equal (combo mod-shift key-9)
         (combo mod-shift key-0) (combo mod-shift key-7) mod-ctrl
         key-backtick key-1 key-2 key-3 key-backslash

         set-layer-2 key-insert mod-super mod-shift key-backspace mod-alt
         key-space fn key-e key-0 key-right-bracket))

(vector-set!
 layers 2 ; TODO
 (vector 0))
