;;; This is the multidvorak layout.

;; It will work for the 44-key Atreus 2 or the 42-key Atreus 1.

;; We have to declare this up front and set it later because of circularity.
(define layers #f)
(define current-layer #f)
(define momentary-layer #f)

(define (fn on?) (set! momentary-layer (and on? (vector-ref layers 1))))

(define (set-layer n)
  (lambda (_) (set! current-layer (vector-ref layers n))))

;; This will reset the board but fails to enter the bootloader for some reason.
(define (reset _) (call-c-func "reset"))

;; On the Atreus 1, we need to expose backtick on the fn layer, but on
;; the Atreus 2 it has its own key, so we put percent there instead.
(define backtick-or-percent
  ;; (sft key-5)
  key-backtick)

;;;; layers

;; NB: the middle keys (ctrl and alt on the 42-key, also ~ and \ on the 44-key
;; variant) are physically in two separate columns, but electrically they are
;; both wired in to the same middle column.

;;; physical location:

;;   ~    \
;; ctrl  alt

;;; electrical arrangement:

;;   ~
;;   \
;;  ctrl
;;  alt

;; This is why it looks like the top two rows should have 10 columns and the
;; bottom should have 12; in reality there are electrically 11 columns.

(define base-layer
  (vector key-q key-w key-e key-r key-t key-backtick
          key-y key-u key-i key-o key-p

          key-a key-s key-d key-f key-g key-backslash
          key-h key-j key-k key-l key-semicolon

          key-z key-x key-c key-v key-b mod-ctrl
          key-n key-m key-comma key-period key-slash

          key-esc key-tab mod-super mod-shift key-backspace mod-alt
          key-space fn key-quote key-left-bracket key-enter))

(define fn-layer
  (vector (sft key-1) (sft key-2) key-up (sft key-4) backtick-or-percent
          (sft key-6) key-page-up key-7 key-8 key-9 key-backspace

          (sft key-9) key-left key-down key-right (sft key-0) (sft key-7)
          key-page-down key-4 key-5 key-6 key-backslash

          key-dash key-equal (sft key-3) (sft key-dash) (sft key-equal) mod-ctrl
          (sft key-8) key-1 key-2 key-3 (sft key-right-bracket)

          (set-layer 2) key-insert mod-super mod-shift key-backspace mod-alt
          key-space fn key-e key-0 key-right-bracket))

(define l2-layer
  (vector key-insert key-home key-up key-end key-page-up 0
          key-up key-f7 key-f8 key-f9 key-f10

          key-delete key-left key-down key-right key-page-down 0
          key-down key-f4 key-f5 key-f6 key-f11

          (set-layer 0) key-vol-up 0 0 reset mod-ctrl
          (set-layer 4) key-f1 key-f2 key-f3 key-f12

          0 key-vol-down mod-super mod-shift key-backspace mod-alt
          key-space (set-layer 0) key-printscreen key-scroll-lock key-pause))

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
  (vector (sft key-1) (sft key-2) key-up (sft key-4) (sft key-5) (sft key-6)
          key-page-up key-7 key-8 key-9 (sft key-backspace)

          (sft key-3) key-left key-down key-right (sft key-4) 0
          key-page-down key-4 key-5 key-6 (sft key-equal)

          key-left-bracket key-right-bracket (sft key-9) (sft key-0) (sft key-7)
          mod-ctrl key-backtick key-1 key-2 key-3 key-backslash

          (set-layer 2) key-insert mod-super mod-shift key-backspace mod-alt
          key-space fn key-e key-0 key-right-bracket))

(set! layers (vector base-layer fn-layer l2-layer
                     hard-dvorak-layer hard-dvorak-fn-layer))
