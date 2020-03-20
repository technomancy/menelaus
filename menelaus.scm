;;; menelaus.scm - a USB keyboard firmware for the Atreus.

;; Note that there are a few unusual style choices made here because
;; it is written in a shared subset of Microscheme and Racket so that it
;; can be tested on a PC without uploading it to a device for every change.

;; For one example, we use `and' where `when' would be more idiomatic. We
;; are also missing the `cond' form.

;; In general when you see an -aux function, it is an internal function which
;; recursively steps thru a vector/list with the initial arguments calculated
;; by its non-aux equivalent.

(include "keycodes.scm")
(include "layout.scm")

;; What are the rows and columns we care about?
(define rows (list 0 1 2 3))
(define columns (list 0 1 2 3 4 5 6 7 8 9 10))

;; Which GPIO pins are responsible for each row or column?
(define row-pins (vector 3 2 1 0))
(define column-pins (vector 6 5 9 8 7 4 10 19 18 12 11))

;; If you have a kit where the PCB is installed upside-down, uncomment this:
;; (set! column-pins (vector 11 12 18 19 10 4 7 8 9 5 6))
;; ;; Upside-down PCB makes the columns backwards but also trades ctrl and alt;
;; ;; this hack only works for layouts where ctrl and alt are in standard place.
;; (set! mod-alt (modify 1))
;; (set! mod-ctrl (modify 3))

;; The above should be handled by a compile-time environment variable but that
;; isn't yet part of Microscheme:
;; https://github.com/ryansuchocki/microscheme/issues/32

;;;;;;;;;;;;;;;;;;; Utility

(define (find-aux v x n max)
  (let ((y (vector-ref v n)))
    (if (and y (= x y))
        n
        (and (< n max)
             (find-aux v x (+ n 1) max)))))

;; Return the index for x in vector v.
(define (find v x)
  (find-aux v x 0 (- (vector-length v) 1)))

(define (remove-aux v lst checked all?)
  (if (null? lst)
      (reverse checked)
      (if (equal? v (car lst))
          (if all?
              (remove-aux v (cdr lst) checked all?)
              (reverse (append (cdr lst) checked)))
          (remove-aux v (cdr lst) (cons (car lst) checked) all?))))

;; Return a copy of lst with the first element equal to v removed.
(define (remove v lst) (remove-aux v lst (list) #f))

;; Return a copy of lst with all elements equal to v removed.
(define (remove-all v lst) (remove-aux v lst (list) #t))

;;;;;;;;;;;;;;;;;;; The Matrix

;; A scan is defined as a list containing the key positions which are currently
;; pressed for a given pass thru the key matrix. We specifically do not attempt
;; to look up what the keys are mapped to yet; we have to do that later on after
;; identifying presses and releases, otherwise we run into layer-switching bugs.
;; Each element in the list is an integer representation of the key in question.

;; Which key in a layout vector is represented by the given row and column?
(define (offset-for row col)
  (+ col (* row (length columns))))

;; Update scan to include the key for the given row/col if it's pressed.
(define (scan-key scan row col)
  (if (and (< (length scan) 10) ; one USB frame can only send 6 keycodes + mods
           ;; pullup resistors mean a closed circuit is low rather than high
           (low? (vector-ref column-pins col)))
      (cons (offset-for row col) scan)
      scan))

;; Step thru every column for a row and ensure it gets scanned.
(define (scan-column scan row columns-left)
  (if (null? columns-left)
      scan
      (scan-column (scan-key scan row (car columns-left))
                   row (cdr columns-left))))

;; Scanning a single column tells us that the key for that column in the active
;; row has been pressed, because the key creates a circuit between the active
;; row's output pin and that column's input pin, causing the output pin's low
;; voltage to overcome the input pin's pullup resistor.
(define (activate-row row)
  (for-each-vector high row-pins)
  (low (vector-ref row-pins row)))

;; For each row, ensure that only its pin is activated, then check every column
;; in that row, consing onto the scan list.
(define (scan-matrix scan rows-left)
  (if (null? rows-left)
      scan
      (begin
        (activate-row (car rows-left))
        (scan-matrix (scan-column scan (car rows-left) columns)
                     (cdr rows-left)))))

;;;;;;;;;;;;;;;;;;; Debouncing

;; Electrical contacts do not switch cleanly from high to low voltage; there is
;; a short period of "bounce" while the signal settles into its new position.
;; In order to counteract this effect, we scan the whole matrix several times,
;; only considering the data we get trustworthy if we get the same value three
;; times in a row.

(define debounce-passes 3)

(define (debounce-matrix-aux last-scan passes-left)
  (if (< 0 passes-left)
      (let ((this-scan (scan-matrix (list) rows)))
        (if (equal? this-scan last-scan)
            (debounce-matrix-aux this-scan (- passes-left 1))
            (debounce-matrix-aux this-scan debounce-passes)))
      last-scan))

(define (debounce-matrix)
  (debounce-matrix-aux (list) debounce-passes))

;;;;;;;;;;;;;;;;;;; Press and release tracking

;; If we didn't have layers, we'd be done now. But since we have layers, we
;; can't assume a 1:1 mapping between keys pressed and keycodes we should send.
;; If you press key 0 on layer 0 where it's bound to Q and then switch to layer
;; one where it's bound to ! then the layer switch shouldn't cause ! to be sent;
;; you should have to release and press key 0 again to trigger that.

;; Fun fact: my original firmware written in C worked around this by just adding
;; a delay to the activation of the layer, which was cringeworthy but kinda
;; sorta worked; better than you would expect anyway:

;; https://github.com/technomancy/atreus-firmware/issues/12
;; https://github.com/technomancy/atreus-firmware/issues/49

;; Because of this, it's necessary to track press and release on the level of
;; physical keys and only map it to keycodes when a new press is detected.

;; Which physical keys were pressed during the last scan?
(define last-keys-down (vector #f #f #f #f #f #f #f #f #f #f))

;; Find an empty slot in last-keys-down to save off the given key in.
(define (add-last-down-aux key n)
  (if (not (vector-ref last-keys-down n))
      (vector-set! last-keys-down n key)
      (and (< n 9) (add-last-down-aux key (+ n 1)))))

;; Remove the given key from the vector of presses from last pass.
(define (remove-last-down-aux key n)
  (if (equal? key (vector-ref last-keys-down n))
      (vector-set! last-keys-down n #f)
      (and (< n 9) (remove-last-down-aux key (+ n 1)))))

(define (add-last-down key) (add-last-down-aux key 0))
(define (remove-last-down key) (remove-last-down-aux key 0))

(define (press/release-aux press release keys-scanned)
  (if (null? keys-scanned)
      (cons press release)
      (let ((key (car keys-scanned)))
        (if (member key release)
            (press/release-aux press (remove key release) (cdr keys-scanned))
            (press/release-aux (cons key press) release (cdr keys-scanned))))))

;; Takes a list of keys from a scan and returns a cons where the car is a list
;; of keys just pressed and the cdr is a list of keys just released.
(define (press/release-for keys-scanned)
  (let ((p/r (press/release-aux (list)
                                (remove-all #f (vector->list last-keys-down))
                                keys-scanned)))
    ;; save off press/release into last-keys-down for next cycle
    (for-each add-last-down (car p/r))
    (for-each remove-last-down (cdr p/r))
    p/r))

;;;;;;;;;;;;;;;;;;; Generating Keycodes

;; Given keys that have been pressed, turn those into keycodes for our USB
;; frame. Given keys that are released, update the press/release tracking
;; data to reflect them.

;; Vectors to store keycodes for the USB frame we are preparing to send.
(define modifiers (vector 0 0 0 0))
(define keycodes-down (vector 0 0 0 0 0 0))

;; For each element of the keycodes-down or modifiers vector, which physical
;; key caused it to be pressed?
(define keys-for-modifiers (vector #f #f #f #f))
(define keys-for-frame (vector #f #f #f #f #f #f))

;; Given a physical key index, what keycode does it map to in the layout?
(define (lookup key-pos)
  (let ((layout (or momentary-layer current-layer)))
    (vector-ref layout key-pos)))

;; Record that a given key resulted in a specific modifier press.
(define (press-modifier keycode key)
  (vector-set! modifiers (- keycode 1) 1)
  (vector-set! keys-for-modifiers (- keycode 1) key))

;; Record that a given key resulted in a specific non-modifier press.
(define (press-normal-key keycode key)
  (let ((slot (find keycodes-down 0)))
    (and slot (vector-set! keycodes-down slot keycode))
    (and slot (vector-set! keys-for-frame slot key))))

;; Record a key press in the modifiers/keycodes-down vectors for the layout.
(define (press-key key)
  (let ((keycode (lookup key)))
    ;; Sometimes "keycodes" are procedures; in that case we call them with
    ;; true when the key is pressed and false when it's released.
    (if (procedure? keycode)
        (keycode #t)
        (if (modifier? keycode)
            (begin (press-modifier (unmodify keycode) key)
                   (if (uncombo keycode)
                       (press-normal-key (uncombo keycode) key)
                       #f))
            (press-normal-key keycode key)))))

;; Record that a given key being released resulted in a modifier release.
(define (release-modifier keycode key n)
  (if (= (or (vector-ref keys-for-modifiers n) (- 0 1)) key)
      (begin
        (vector-set! modifiers n 0)
        (vector-set! keys-for-modifiers n #f))
      (and (< n 3) (release-modifier keycode key (+ n 1)))))

;; Record a key release, clearing it out of the press tracking data.
(define (release-key key)
  ;; lookup here looks it up in the current layer, even if it was pressed in
  ;; the momentary layer. these need to be consistent across layers or tracked
  ;; in a similar manner as keys-for-frame.
  (let ((keycode (lookup key)))
    (if (procedure? keycode)
        (keycode #f)
        (let ((slot (find keys-for-frame key))
              (modifier-slot (find keys-for-modifiers key)))
          (if slot
              (begin
                (vector-set! keycodes-down slot 0)
                (vector-set! keys-for-frame slot #f))
              #f)
          (if modifier-slot
              (release-modifier modifier-slot key 0)
              #f)))))

;;;;;;;;;;;;;;;;;;; SHOWTIME

;; Prepare the GPIO pins and initialize the USB connection.
(define (init)
  (set! current-layer (vector-ref layers 0))
  (for-each-vector output row-pins)
  (for-each-vector high row-pins)
  (for-each-vector input column-pins)
  (for-each-vector high column-pins) ; activate pullup resistors

  (call-c-func "usb_init")
  (pause 200))

;; Take press/release data and set USB keycodes and modifiers.
(define (set-usb-frame press/release)
  (let ((press (car press/release))
        (release (cdr press/release)))
    (for-each press-key press)
    (for-each release-key release)))

;; Actually send the USB frame.
(define (usb-send m k0 k1 k2 k3 k4 k5)
  (let ((mods (+ (vector-ref m 0) (* (vector-ref m 1) 2)))) ; + isn't variadic
    (let ((mods (+ mods (+ (* (vector-ref m 2) 4) (* (vector-ref m 3) 8)))))
      ;; call-c-func is a special form and cannot be applied
      (call-c-func "usb_send" mods k0 k1 k2 k3 k4 k5))))

;; Scan the matrix, determine the appropriate keycodes, and send them.
(define (loop)
  (free! (let ((keys-scanned (debounce-matrix)))
           (set-usb-frame (press/release-for keys-scanned))
           (apply usb-send (cons modifiers (vector->list keycodes-down)))))
  (loop))

(init)
(loop)
