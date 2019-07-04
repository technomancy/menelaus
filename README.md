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
* ~200 lines of code

## Usage

This currently requires Microscheme with the addition of
`vector-copy!` which at the time of this writing is only on the master branch.

Also requires [avrdude](https://www.nongnu.org/avrdude/) for uploading
to the device.

Replace `/dev/ttyACM0` with the path your OS assigns to the USB
bootloader of the microcontroller:

    $ make upload USB=/dev/ttyACM0

On Mac OS X sometimes the USB path is `/dev/cu.usbmodem1411` or similar.

Currently only the "multidvorak" layout is included.

## Development

The firmware can also be run on a PC rather than on the
microcontroller in the keyboard using `test.rkt` which loads it up
into Racket and simulates the GPIO functions with a test harness:

    $ make test

## Known bugs

Still working out some quirks with sticky layers.

If you hold the fn key, press a button (say Q) and then release fn
without releasing Q, it will send a keycode for Q rather than simply
leaving the previous fn+Q keycodes as held down.

## License

Copyright © 2014-2019 Phil Hagelberg and contributors

Released under the [GNU GPL version 3](https://www.gnu.org/licenses/gpl.html).

Uses [PJRC USB Keyboard library](http://www.pjrc.com/teensy/usb_keyboard.html)
which is Copyright © 2009 PJRC.COM, LLC and released under the MIT/X11 license.
