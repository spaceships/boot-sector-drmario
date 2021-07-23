run: drmario.img
	qemu-system-i386 -hda $^

palette: palette.img
	qemu-system-i386 -hda $^

debug: drmario.img
	qemu-system-i386 -s -S -hda $^

gdb: drmario.elf
	gdb $< \
			-ex 'target remote localhost:1234' \
			-ex 'set architecture i8086' \
			-ex 'layout regs' \
			-ex 'break start' \
			-ex 'continue'

size: drmario.asm
	@perl -nE 'print unless /times 510|dw 0xaa55/' $^ > tmp.asm
	@nasm -f elf32 -g3 -F dwarf tmp.asm -o tmp.o
	@ld.lld -Ttext=0x7c00 -melf_i386 tmp.o -o tmp.elf 2>/dev/null
	@llvm-objcopy -O binary tmp.elf tmp.img
	@wc -c < tmp.img
	@rm tmp.*

%.o: %.asm
	nasm -f elf32 -g3 -F dwarf $^ -o $@

%.elf: %.o
	ld.lld -Ttext=0x7c00 -melf_i386 $^ -o $@

%.img: %.elf
	llvm-objcopy -O binary $^ $@

clean:
	rm -rf *.img *.elf *.o

.PHONY: run clean drun size
