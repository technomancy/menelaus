# Menelaus

A firmware for the
[Atreus](http://atreus.technomancy.us) keyboard, written in
[Microscheme](https://ryansuchocki.github.io/microscheme/).

## Features

* 6KRO (6 simultaneous keys, not including modifiers)
* Software debouncing
* Multiple layers (limited only by memory)
* Bind arbitrary Scheme functions to a key
* Combo keys (a single keystroke can send a modifier and a non-modifier)

## Usage

Replace `/dev/ttyACM0` with the path your OS assigns to the USB
bootloader of the microcontroller:

    $ make upload USB=/dev/ttyACM0

Currently only the "multidvorak" layout is supported.

## Development

The firmware can also be run on a PC rather than on the
microcontroller in the keyboard using `test.rkt` which loads it up
into Racket and simulates the GPIO functions with a test harness:

    $ make test

## License

Copyright © 2014-2019 Phil Hagelberg and contributors

Released under the [GNU GPL version 3](https://www.gnu.org/licenses/gpl.html).
