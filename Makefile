MCU=atmega32u4

F_CPU=16000000

TARGET=menelaus

USB_DEVICE=/dev/ttyACM0

build: $(TARGET).hex

upload: $(TARGET).hex
	while [ ! -r $(USB_DEVICE) ]; do sleep 1; done; \
	avrdude -p $(MCU) -c avr109 -U flash:w:$(TARGET).hex -P $(USB_DEVICE)

test: ; racket test.rkt

clean:
	-rm -f $(TARGET){,.hex} *.o *.elf *.s

$(TARGET).hex: $(TARGET).elf
	avr-size $(TARGET).elf
	avr-objcopy --output-target=ihex $(TARGET).elf $(TARGET).hex

%.s: %.scm
	microscheme -m LEO $(TARGET).scm

%.elf: %.s usb_keyboard.s
	avr-gcc -mmcu=$(MCU) -o $(TARGET).elf $(TARGET).s usb_keyboard.s

usb_keyboard.s: usb_keyboard.h usb_keyboard.c
	avr-gcc -std=gnu99 -S -D F_CPU=$(F_CPU)UL -mmcu=$(MCU) -c \
	  -o usb_keyboard.s usb_keyboard.c

.PHONY: build upload test clean
