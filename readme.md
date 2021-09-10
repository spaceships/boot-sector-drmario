Boot Sector Dr. Mario
=====================

![gameplay gif](https://gitlab-ext.galois.com/bcarmer/boot-sector-drmario/raw/master/game.gif "Dr Mario")

This is an implementation of Dr. Mario in 16-bit x86. It is an attempt to make Dr. Mario
fit within the 512 byte boot sector. It uses BIOS interrupts to do things like set video
mode. It is currently a bit larger than 512 bytes!

Dependencies: qemu-system-i386, nasm.

Controls are the arrow keys and 'a' and 's'.

To play it, run 
```
make run
```

To find the current size of it
```
make size
```

Contributors
============
* Brent Carmer (author)
* Brian Huffman (optimization wizard)
* Daniel Wagner (optimzation contributor)
* Stuart Pernsteiner (optimzation contributor)
