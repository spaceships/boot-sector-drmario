ifeq (run, $(firstword $(MAKECMDGOALS)))
  RUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(RUN_ARGS):;@:)
endif

ifeq (drun, $(firstword $(MAKECMDGOALS)))
  DRUN_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(DRUN_ARGS):;@:)
endif

ifeq (gdb, $(firstword $(MAKECMDGOALS)))
  GDB_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(GDB_ARGS):;@:)
endif

ifeq (size, $(firstword $(MAKECMDGOALS)))
  SIZE_ARGS := $(wordlist 2,$(words $(MAKECMDGOALS)),$(MAKECMDGOALS))
  $(eval $(SIZE_ARGS):;@:)
endif

run: $(patsubst %,%.img,$(RUN_ARGS))
	# qemu-system-i386 -s -S -hda $^
	qemu-system-i386 -hda $^

drun: $(patsubst %,%.img,$(DRUN_ARGS))
	qemu-system-i386 -s -S -hda $^

gdb: $(patsubst %,%.elf,$(GDB_ARGS))
	gdb $< \
			-ex 'target remote localhost:1234' \
			-ex 'set architecture i8086' \
			-ex 'layout regs' \
			-ex 'break start' \
			-ex 'continue'

size: $(SIZE_ARGS)
	@perl -nE 'print unless /times 510|dw 0xaa55/' $^.asm > tmp.asm
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
