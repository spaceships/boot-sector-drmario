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

%.o: %.asm
	nasm -f elf32 -g3 -F dwarf $^ -o $@

%.elf: %.o
	ld.lld -Ttext=0x7c00 -melf_i386 $^ -o $@

%.img: %.elf
	llvm-objcopy -O binary $^ $@

clean:
	rm -rf *.img *.elf *.o

.PHONY: run clean
