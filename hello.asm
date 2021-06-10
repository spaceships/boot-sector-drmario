    bits 16

start:
    mov bx, string
    call display_string
    hlt

string: 
    db 0x0d,0x0a,"Hello, world",0x0d,0x0a,0

%include "lib.asm"
%include "end.asm"
