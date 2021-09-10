run: drmario.bin
	qemu-system-i386 -hda $^

dis: drmario.bin
	ndisasm -b16 -o0x7c00 $^

size: drmario.asm
	@perl -nE 'print unless /times 510|dw 0xaa55/' $^ > tmp.asm
	@perl -pi -E 's/enable_load 1/enable_load 0/' tmp.asm
	@nasm tmp.asm -o tmp.bin
	@wc -c < tmp.bin
	@rm tmp.*

%.bin: %.asm
	nasm -o $@ $^

clean:
	rm -rf *.bin

.PHONY: run clean size dis
