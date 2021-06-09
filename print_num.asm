    bits 16

start:
    mov ax,0xbeef
    call display_number
    mov bx,string
    call display_string
    int 0x20

string: 
    db 0x0d,0x0a,"Done",0x0d,0x0a,0

%include "lib.asm"
%include "end.asm"
