    bits 16


v_a:    equ 0xfa00  ; y coordinate
v_b:    equ 0xfa02  ; x coordinate
v_x:    equ 0xfa04  ; x 32-bit for Mandelbrot 24.8 fraction
v_y:    equ 0xfa08  ; y 32-bit for Mandelbrot 24.8 fraction
v_s1:   equ 0xfa0c  ; temporal s1
v_s2:   equ 0xfa10  ; temporal s2 (48 bit - 6 bytes)

start:
    mov ax,0x0013
    int 0x10

    mov ax,0xa000
    mov ds,ax
    mov es,ax
    
m4:
    mov ax,199
    mov [v_a],ax
m0: mov ax,319
    mov [v_b],ax
m1: xor ax,ax
    mov [v_x],ax    ; x = 0.0
    mov [v_x+2],ax
    mov [v_y],ax    ; y = 0.0
    mov [v_y+2],ax
    mov cx,0

m2: push cx         ; save counter
    mov ax,[v_x]    ; read x
    mov dx,[v_x+2]
    call square32  ; get x^2
    push dx         ; save result to stack
    push ax
    mov ax,[v_y]    ; read y
    mov dx,[v_y+2]
    call square32  ; get y^2

    pop bx          ; add both (x^y + y^2)
    add ax,bx
    pop bx
    adc dx,bx

    pop cx          ; restore counter
    cmp dx,0        ; result is >= 4.0?
    jne m3
    cmp ax,4*256
    jnc m3

    push cx
    mov ax,[v_y]    ; read y
    mov dx,[v_y+2]
    call square32  ; get y^2
    push dx
    push ax
    mov ax,[v_x]    ; read x
    mov dx,[v_x+2]
    call square32  ; get x^2

    pop bx         
    sub ax,bx       ; sub x^2 - y^2
    pop bx
    sbb dx,bx

    add ax,[v_b]
    adc dx,0
    add ax,[v_b]
    adc dx,0
    sub ax,480
    sbb dx,0

    push ax
    push dx

    mov ax,[v_x]
    mov dx,[v_x+2]
    mov bx,[v_y]
    mov cx,[v_y+2]
    call mul32

    shl ax,1
    rcl dx,1

    add ax,[v_a]
    adc dx,0
    add ax,[v_a]
    adc dx,0
    sub ax,250
    sbb dx,0

    mov [v_y],ax
    mov [v_y+2],dx

    pop dx
    pop ax

    mov [v_x],ax ; save as new x value
    mov [v_x+2],dx

    pop cx
    inc cx
    cmp cx,100
    je m3
    jmp m2

m3: mov ax,[v_a]    ; get y-coordinate
    mov dx,320      ; mult by 320 (size of pixel row)
    mul dx
    add ax,[v_b]    ; add x coordinate to result
    xchg ax,di

    add cl,0x20     ; index counter into rainbow colors
    mov [di],cl     ; put pixel on screen
    
    dec word [v_b]  ; decrease column
    jns m1          ; is it negative?

    dec word [v_a]  ; decrease row
    jns m0

    mov ah,0x00
    int 0x16

    mov ax,0x0002
    int 0x10

    int 0x20


square32:  
    mov bx,ax
    mov cx,dx

mul32:
    xor dx,cx
    pushf
    xor dx,cx
    jns mul32_2
    not ax
    not dx
    add ax,1
    adc dx,0

mul32_2:
    test cx,cx
    jns mul32_3
    not bx
    not cx
    add bx,1
    adc cx,0

mul32_3:
    mov [v_s1],ax
    mov [v_s1+2],dx
    mul bx
    mov [v_s2],ax
    mov [v_s2+2],dx

    mov ax,[v_s1+2]
    mul cx
    mov [v_s2+4],ax
    mov ax,[v_s1+2]
    mul bx
    add [v_s2+2],ax
    adc [v_s2+4],dx
    mov ax,[v_s1]
    mul cx
    add [v_s2+2],ax
    adc [v_s2+4], dx

    mov ax,[v_s2+1]
    mov dx,[v_s2+3]

    popf
    jns mul32_1
    not ax
    not dx
    add ax,1
    adc dx,0
mul32_1:
    ret 


%include "end.asm"
