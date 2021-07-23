; from Boot Sector Games

    bits 16

v_a:    equ 0xfa00
v_b:    equ 0xfa02

start:
    mov ax,0x0013
    int 0x10

    mov ax,0xa000
    mov ds,ax
    mov es,ax

m4: 
    mov ax,127
    mov [v_a],ax
m0: mov ax,127
    mov [v_b],ax

m1:
    mov ax,[v_a]
    mov dx,320
    mul dx
    add ax,[v_b]
    xchg ax,di

    mov ax,[v_a]
    and ax,0x78
    add ax,ax

    mov bx,[v_b]
    and bx,0x78
    mov cl,3
    shr bx,cl
    add ax,bx
    stosb

    dec word [v_b]
    jns m1

    dec word [v_a]
    jns m0

    mov ah,0x00
    int 0x16

    mov ax,0x002
    int 0x10

    int 0x20

times 510 - ($ - $$) db 0
dw 0xaa55
