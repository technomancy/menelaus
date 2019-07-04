MCU=atmega32u4

F_CPU=16000000

TARGET=menelaus

USB=/dev/ttyACM0

build: $(TARGET).hex

upload: $(TARGET).hex
	while [ ! -r $(USB) ]; do sleep 1; done; \
	avrdude -p $(MCU) -c avr109 -U flash:w:$(TARGET).hex -P $(USB)

test: ; racket test.rkt

clean: ; -rm -f $(TARGET){,.hex} *.o *.elf *.s

count: ; cloc menelaus.scm keycodes.scm

$(TARGET).hex: $(TARGET).elf
	avr-size $(TARGET).elf
	avr-objcopy --output-target=ihex $(TARGET).elf $(TARGET).hex

$(TARGET).s: $(TARGET).scm layout.scm keycodes.scm
	microscheme -m LEO $(TARGET).scm

%.elf: %.s usb_keyboard.s
	avr-gcc -mmcu=$(MCU) -o $(TARGET).elf $(TARGET).s usb_keyboard.s

usb_keyboard.s: usb_keyboard.h usb_keyboard.c
	avr-gcc -std=gnu99 -S -D F_CPU=$(F_CPU)UL -mmcu=$(MCU) -c \
	  -o usb_keyboard.s usb_keyboard.c

.PHONY: build upload test clean count
