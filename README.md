# Menelaus

A firmware for the
[Atreus](http://atreus.technomancy.us) keyboard, written in
[Microscheme](https://ryansuchocki.github.io/microscheme/).

See [this article about how it works](https://atreus.technomancy.us/firmware).

## Features

* 6KRO (6 simultaneous keys, plus 4 modifiers)
* Software debouncing
* Multiple layers, momentary and sticky (limited only by memory)
* Combo keys (a single keystroke can send a modifier and a non-modifier)
* Bind arbitrary Microscheme functions to a key
* ~300 lines of code

## Usage

Install [microscheme](https://github.com/ryansuchocki/microscheme/)
from source; place `microscheme` executable on your `$PATH`. Version
823c5d9 from February 2020 is known to work.

Requires [avrdude](https://www.nongnu.org/avrdude/) for uploading
to the controller on the keyboard; install with your package manager
of choice.

Replace `/dev/ttyACM0` with the path your OS assigns to the USB
bootloader of the microcontroller (on Mac OS X sometimes it is
`/dev/cu.usbmodem1411` or similar):

    $ make upload USB=/dev/ttyACM0

Once you run that, put the device in bootloader mode; sometimes this
can be invoked by a key combo and sometimes a hard reset is
necessary. On the A-star Micro used in the Atreus kits, this is done
by shorting GND and RST twice in under a second, which causes the
onboard LED to pulse. The Keyboardio Atreus has a reset button you can
press with a pin to the bottom of the board.

## Known bugs

The reset function in the firmware has no effect; hard-reset must be
used to flash a new firmware once this is uploaded.

## Layout

By default you get the "multidvorak" layout which is designed to send
the right keycodes with the assumption that the OS is set to use
Dvorak, but it also includes layers for "hard Dvorak". But you can
also build a qwerty layout:

    $ cp qwerty.scm layout.scm
    $ make upload USB=/dev/ttyACM0

Or edit `layout.scm` to your liking; you can see a list of available
keycodes in `keycodes.scm`. The default layout works for 42-key Atreus
kits and the 44-key Keyboardio Atreus, but you will have to uncomment
a few things for the full 44-key support.

## Development

The firmware can also be run on a PC rather than on the
microcontroller in the keyboard using `test.rkt` which loads it up
into Racket and simulates the GPIO functions with a test harness:

    $ make test
    racket test.rkt
    ..........................

## License

Copyright © 2014-2020 Phil Hagelberg and contributors

Released under the [GNU GPL version 3](https://www.gnu.org/licenses/gpl.html).

Uses [PJRC USB Keyboard library](http://www.pjrc.com/teensy/usb_keyboard.html)
which is Copyright © 2009 PJRC.COM, LLC and released under the MIT/X11 license.
