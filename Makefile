MCU=atmega32u4
F_CPU=16000000

LAYOUT?=qwerty

USB=/dev/ttyACM0

build: $(LAYOUT).hex

upload: $(LAYOUT).hex
	echo "Put your device in bootloader mode now..."
	echo "Classic Atreus: connect GND pin to RST pin twice in under a secod."
	echo "Keyboardio Atreus: press the button on the underside of the board."
	while [ ! -r $(USB) ]; do sleep 1; done; \
	avrdude -p $(MCU) -c avr109 -U flash:w:$(LAYOUT).hex -P $(USB)

test: ; racket test.rkt

clean: ; -rm -f $(LAYOUT){,.hex} *.o *.elf *.s

count: ; cloc menelaus.scm keycodes.scm $(LAYOUT).scm

$(LAYOUT).hex: $(LAYOUT).elf
	avr-size $(LAYOUT).elf
	avr-objcopy --output-target=ihex $(LAYOUT).elf $(LAYOUT).hex

$(LAYOUT).s: $(LAYOUT).scm menelaus.scm keycodes.scm
	microscheme -m LEO $(LAYOUT).scm

%.elf: %.s usb_keyboard.s
	avr-gcc -mmcu=$(MCU) -o $(LAYOUT).elf $(LAYOUT).s usb_keyboard.s

usb_keyboard.s: usb_keyboard.h usb_keyboard.c
	avr-gcc -std=gnu99 -S -D F_CPU=$(F_CPU)UL -mmcu=$(MCU) -c \
	  -o usb_keyboard.s usb_keyboard.c

udev: /etc/udev/rules.d/a-star.rules

/etc/udev/rules.d/a-star.rules:
	echo "SUBSYSTEM==\"usb\", ATTRS{idVendor}==\"1ffb\", \
	  ATTRS{idProduct}==\"0101\", ENV{ID_MM_DEVICE_IGNORE}=\"1\"" > $@
	echo "SUBSYSTEM==\"usb\", ATTRS{idVendor}==\"1ffb\", \
	  ATTRS{idProduct}==\"2300\", ENV{ID_MM_DEVICE_IGNORE}=\"1\"" >> $@

.PHONY: build upload test clean count udev
