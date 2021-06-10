    bits 16

table:          equ 0x8000
table_size:     equ 10000

start:
    mov bx,table
    mov cx,table_size
    mov al,0

p1: mov [bx],al
    inc bx
    loop p1

    mov ah,0
    mov bx,table
    mov cx,table_size

    mov ax,2
p2: mov bx,table
    add bx,ax
    cmp byte [bx],0
    jne p3
    push ax
    call display_number
    mov al,','
    call display_letter
    pop ax

p4: add bx,ax
    cmp bx,table+table_size
    jnc p3
    mov byte [bx],1
    jmp p4

p3: inc ax
    cmp ax,table_size
    jne p2

    hlt


%include "lib.asm"
%include "end.asm"
