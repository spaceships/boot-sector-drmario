run: drmario.bin
	qemu-system-i386 -hda $^

palette: palette.img
	qemu-system-i386 -hda $^

dis: drmario.bin
	ndisasm -b16 -o0x7c00 $^

size: drmario.asm
	@perl -nE 'print unless /times 510|dw 0xaa55/' $^ > tmp.asm
	@nasm tmp.asm -o tmp.bin
	@wc -c < tmp.bin
	@rm tmp.*

%.bin: %.asm
	nasm -o $@ $^

clean:
	rm -rf *.bin

.PHONY: run clean size dis
