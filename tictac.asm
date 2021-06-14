    bits 16

board: equ 0x0300

start:
    mov bx,board
    mov cx,9
    mov al,'1'
b09:
    mov [bx],al
    inc al
    inc bx
    loop b09
b10:
    call show_board
    call get_movement
    mov byte [bx],'X'
    call show_board
    call get_movement
    mov byte [bx],'O'
    jmp b10

end:
    hlt
    jmp end

show_board:
    mov bx,board
    call show_row
    call show_div
    mov bx,board+3
    call show_row
    call show_div
    mov bx,board+6
    jmp show_row

show_row:
    call show_square
    mov al,0x7c
    call display_letter
    call show_square
    mov al,0x7c
    call display_letter
    call show_square
show_crlf:
    mov al,0x0d
    call display_letter
    mov al,0x0a
    jmp display_letter

show_div:
    mov al,0x2d
    call display_letter
    mov al,0x2b
    call display_letter
    mov al,0x2d
    call display_letter
    mov al,0x2b
    call display_letter
    mov al,0x2d
    call display_letter
    jmp show_crlf

show_square:
    mov al,[bx]
    inc bx
    jmp display_letter

get_movement:
    call read_keyboard
    cmp al,0x1b
    je end
    sub al,'1'
    jc get_movement
    cmp al,0x09
    jnc get_movement
    cbw
    mov bx,board
    add bx,ax
    mov al,[bx]
    cmp al,0x40
    jnc get_movement
    call show_crlf
    ret


%include "lib.asm"
%include "end.asm"
