Boot Sector Dr. Mario
=====================

![gameplay gif](https://gitlab-ext.galois.com/bcarmer/boot-sector-drmario/raw/master/game.gif "Dr Mario")

This is an implementation of Dr. Mario in 16-bit x86. It is an attempt to make Dr. Mario
fit within the 512 byte boot sector. It uses BIOS interrupts to do things like set video
mode. It is currently a bit larger than 512 bytes, so it cheats and loads the other sectors from disk. **I am looking for collaborators to get it down to a single sector!**

Dependencies: `qemu-system-i386`, `nasm`.

Controls are the arrow keys and 'a' and 's'.

To play it, run 
```
make run
```

To find the current size of it
```
make size
```

I wouldn't have even known about boot sector games without stumbling on the Oscar
Toledo G.'s book [Programming Boot Sector
Games](https://www.amazon.com/Programming-Sector-Games-Toledo-Gutierrez/dp/0359816312).
Many of the tricks used in this program came from there.

Contributors
============
* Brent Carmer (author)
* Brian Huffman (optimization wizard)
* Daniel Wagner (optimzation contributor)
* Stuart Pernsteiner (optimzation contributor)
