display_letter:
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    mov ah,0x0e
    mov bx,0x000f
    int 0x10
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

read_keyboard:
    push bx
    push cx
    push dx
    push si
    push di
    mov ah,0x00
    int 0x16
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    ret

; print string in bx
display_string: 
    push ax
    push bx
    push cx
    push dx
    push si
    push di
ds_repeat:
    mov al,[bx]
    test al,al
    je ds_end
    call display_letter
    inc bx
    jmp ds_repeat
ds_end:
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

display_number: 
    mov dx,0
    mov cx,10
    div cx
    push dx
    cmp ax,0
    je display_number_1
    call display_number
display_number_1:
    pop ax
    add al,'0'
    call display_letter
    ret
