    bits 16

start:
    mov bx, string
    call display_string
    int 0x20

string: 
    db 0x0d,0x0a,"Hello, world",0x0d,0x0a,0

%include "lib.asm"
%include "end.asm"
