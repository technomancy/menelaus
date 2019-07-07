# Menelaus

A firmware for the
[Atreus](http://atreus.technomancy.us) keyboard, written in
[Microscheme](https://ryansuchocki.github.io/microscheme/).

## Features

* 6KRO (6 simultaneous keys, not including modifiers)
* Software debouncing
* Multiple layers, momentary and sticky (limited only by memory)
* Combo keys (a single keystroke can send a modifier and a non-modifier)
* Bind arbitrary Scheme functions to a key
* ~250 lines of code

## Usage

This requires [avrdude](https://www.nongnu.org/avrdude/) for uploading
to the controller on the keyboard; install with your package manager
of choice.

Replace `/dev/ttyACM0` with the path your OS assigns to the USB
bootloader of the microcontroller (on Mac OS X sometimes it is
`/dev/cu.usbmodem1411` or similar):

    $ make upload USB=/dev/ttyACM0

By default you get the "multidvorak" layout, but you can also build a
qwerty layout:

    $ cp qwerty.scm layout.scm
    $ make upload USB=/dev/ttyACM0

Or edit `layout.scm` to your liking; you can see a list of available
keycodes in `keycodes.scm`.

## Development

The firmware can also be run on a PC rather than on the
microcontroller in the keyboard using `test.rkt` which loads it up
into Racket and simulates the GPIO functions with a test harness:

    $ make test
    racket test.rkt
    ..........................

## Known bugs

If you hold down two keys which contain a modifier (for instance,
shift and !) and release one of them, it will count as if both are released.

## License

Copyright © 2014-2019 Phil Hagelberg and contributors

Released under the [GNU GPL version 3](https://www.gnu.org/licenses/gpl.html).

Uses [PJRC USB Keyboard library](http://www.pjrc.com/teensy/usb_keyboard.html)
which is Copyright © 2009 PJRC.COM, LLC and released under the MIT/X11 license.
