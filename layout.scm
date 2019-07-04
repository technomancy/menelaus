;;; this is the multidvorak layout

;; we have to declare this up front and set it later because of circularity
(define layers #f)
(define current-layer #f)
(define momentary-layer #f)

(define (fn)
  (set! momentary-layer (vector-ref layers 1)))

(define (set-layer n)
  (lambda () (set! current-layer (vector-ref layers n))))

(define (reset) (call-c-func "reset"))

;;;; layers

(define base-layer
 (vector key-q key-w key-e key-r key-t key-backslash
         key-y key-u key-i key-o key-p

         key-a key-s key-d key-f key-g key-backtick
         key-h key-j key-k key-l key-semicolon

         key-z key-x key-c key-v key-b mod-ctrl
         key-n key-m key-comma key-period key-slash

         key-esc key-tab mod-super mod-shift key-backspace mod-alt
         key-space fn key-quote key-left-bracket key-enter))

(define fn-layer
 (vector (sft key-1) (sft key-2) key-up (sft key-dash) (sft key-equal) 0
         key-page-up key-7 key-8 key-9 (sft key-8)

         (sft key-3) key-left key-down key-right (sft key-4) 0
         key-page-down key-4 key-5 key-6 (sft key-right-bracket)

         key-dash key-equal (sft key-9) (sft key-0) (sft key-7) mod-ctrl
         key-backtick key-1 key-2 key-3 key-backslash

         ;; still got some bugs in layering to work out! make this reset for now
         reset ;; (set-layer 2)
         key-insert mod-super mod-shift key-backspace mod-alt
         key-space fn key-e key-0 key-right-bracket))

(define l2-layer
 (vector key-insert key-home key-up key-end key-page-up 0
         key-up key-f7 key-f8 key-f9 key-f10

         key-delete key-left key-down key-right key-page-down 0
         key-down key-f4 key-f5 key-f6 key-f11

         (set-layer 0) key-vol-up 0 0 reset mod-ctrl
         (set-layer 4) key-f1 key-f2 key-f3 key-f12

         0 key-vol-down mod-super mod-shift key-backspace mod-alt
         key-space 0 key-printscreen key-scroll-lock key-pause))

(define hard-dvorak-layer
 (vector key-quote key-comma key-period key-p key-y key-backslash
         key-f key-g key-c key-r key-l

         key-a key-o key-e key-u key-i key-backtick
         key-d key-h key-t key-n key-s

         key-semicolon key-q key-j key-k key-x mod-ctrl
         key-b key-m key-w key-v key-z

         key-esc key-tab mod-super mod-shift key-backspace mod-alt
         key-space fn key-quote key-left-bracket key-enter))

(define hard-dvorak-fn-layer
 (vector (sft key-1) (sft key-2) key-up (sft key-left-bracket) (sft key-right-bracket) 0
         key-page-up key-7 key-8 key-9 (sft key-8)

         (sft key-3) key-left key-down key-right (sft key-4) 0
         key-page-down key-4 key-5 key-6 (sft key-equal)

         key-left-bracket key-right-bracket (sft key-9) (sft key-0) (sft key-7) mod-ctrl
         key-backtick key-1 key-2 key-3 key-backslash

         (set-layer 2) key-insert mod-super mod-shift key-backspace mod-alt
         key-space fn key-e key-0 key-right-bracket))

(set! layers (vector base-layer fn-layer l2-layer
                     hard-dvorak-layer hard-dvorak-fn-layer))
