    bits 16

base:           equ 0xfc80
sprite_color:   equ base                ; [byte]
cur_pill_loc:   equ sprite_color + 1    ; [word]
cur_pill_rot:   equ cur_pill_loc + 2    ; [byte]
next_tick:      equ cur_pill_rot + 1    ; [word]
rand:           equ next_tick + 2       ; [word]

BLACK:          equ 0xFF
PILL_RED:       equ 0x27
PILL_YELLOW:    equ 0x2c
PILL_BLUE:      equ 0x36
BORDER_COLOR:   equ 0x6a
SPEED:          equ 4
VIRUS_COUNT:    equ 8

BOARD_START:    equ 5*8*320 + 16*8 ; start board at 16,5

start:
    mov ax,0x0013   ; set video mode vga 320x200x256
    int 0x10        ; call bios interrupt

    mov ax,0xa000   ; set data & extra segments to video
    mov ds,ax
    mov es,ax

    mov ah,0x00
    int 0x1a      ; bios clock read
    mov [rand],dx ; initialize rand

    call place_virii
    call draw_outline

game_loop:
    mov ah,0x00
    int 0x1a    ; bios clock read
    cmp dx,[next_tick]
    jb game_loop
    add dx,SPEED
    mov [next_tick],dx

    cmp word [cur_pill_loc],0
    jne gl_pill
    call new_pill
gl_pill:
    call pillfall
    jmp game_loop

end:
    hlt
    jmp end

pillfall:
    mov bx,[cur_pill_loc]
    ; Checking whether we are done
    cmp bx,BOARD_START+16*8*320 ; are we at the bottom row?
    jc pf_keep_checking                          ; if y is not 0, keep going
pf_stop_falling:
    mov word [cur_pill_loc],0   ; stop falling
    jmp pf_done
pf_keep_checking:
    cmp byte [bx+8*320],0       ; is there something below?
    jne pf_stop_falling         ; if there is, stop falling
    ; If pill is horizontal, check if right is filled too
    and byte [cur_pill_rot],1   ; is cur_pill_rot odd? 
    jnz pf_fall_virt            ; if odd, fall vertically
    cmp byte [bx+8*320+8],0 ; otherwise is there something on the right, below?
    jne pf_stop_falling         ; if there is, stop falling
    ; fall horizontally
    push 8  ; other part of sprite is to the right
    jmp pf_fall
pf_fall_virt:
    push -320*8 ; other part of sprite is above
pf_fall:
    mov si,bx
    call move_sprite_down
    pop cx
    add bx,cx
    mov si,bx
    call move_sprite_down
    add word [cur_pill_loc],320*8
pf_done:
    ret

; si: source
move_sprite_down:
    mov di,si
    add di,320*8
    mov cx,8
csd_outer_loop:
    push cx
    mov cx,8
csd_inner_loop:
    movsb ; [di++] = [si++]
    mov byte [si-1],0
    loop csd_inner_loop
    add si,312
    add di,312
    pop cx
    loop csd_outer_loop
    ret
     
; lfsr, sets ax, clobbers bx
rng:
    mov ax,[rand]
    mov bx,ax
    and bx,0b1000000000101101
    shr ax,1
    xor bl,bh
    jpe rng_done
    or ax,0x8000
rng_done:
    mov [rand],ax
    ret

; put a random color from colors array into ah
; mangles ax,bx,dx
rand_color:
    call rng
    and al,3
    jz rand_color
    mov bx,colors-1
    cs xlat     ; al = [colors + al]
    mov ah,al
    ret

new_pill:
    mov si,sprites+2*8
    mov di,BOARD_START+3*8
    call draw_sprite
    mov di,BOARD_START+4*8
    call draw_sprite
    mov word [cur_pill_loc],BOARD_START+3*8
    mov byte [cur_pill_rot],0
    ret

place_virii:
    mov dx,0 ; bx: virus count
    mov di,BOARD_START+15*8*320+7*8 ; start at bottom right
pv_row:
    mov cx,8
pv_loop:
    call rng
    cmp al,210
    jb pv_continue
    push cx
    push di
    mov si,sprites+6*8
    call draw_sprite
    pop di
    pop cx
    inc dx; increment virus count
pv_continue:
    cmp dx,VIRUS_COUNT
    je pv_done
    sub di,8
    ; if cx is 0, then subtract row
    loop pv_loop
    sub di,8*320-8*8
    cmp di,BOARD_START
    ja pv_row
pv_done:
    ret

; draw_sprite gives the sprite a random color
; di: top left pixel
; si; start of sprite
; clobbered: ax, bx, cx, di, si
draw_sprite:
    call rand_color
    mov cx,8
ds_row:
    push cx
    mov cx,8
    cs lodsb        ; load the whole byte of the sprite into al, advance si
    mov bl,al       ; save it in dl
ds_col:
    mov al,BLACK
    shl bl,1
    jnc ds_print    ; if we just shifted off a 0, print black pixel
    mov al,ah       ; otherwise get the color
ds_print:
    stosb           ; print color to current pixel loc
    loop ds_col
    add di,312      ; increment di
    pop cx
    loop ds_row     ; if row != 8, jump to ds_row
    ret

draw_outline:
    mov al,BORDER_COLOR
    mov di,BOARD_START-320-1
    mov cx,16*8+1
do_virt_lines:
    stosb
    add di,8*8
    stosb
    add di,320-8*8-2
    loop do_virt_lines
    mov di,BOARD_START-320-1
    mov cx,8*8+2
    rep stosb
    mov di,BOARD_START+16*8*320-1
    mov cx,8*8+2
    rep stosb
    ret

sprites:
    ; 0: pill top
    db 0b01111100 
    db 0b11011110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b11111110
    db 0b00000000
    ; 1: pill bottom
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b11111110
    db 0b01111100
    db 0b00000000
    ; 2: pill left
    db 0b01111110 
    db 0b11000010
    db 0b10111110
    db 0b11111110
    db 0b11111110
    db 0b11111110
    db 0b01111110
    db 0b00000000
    ; 3: pill right
    db 0b11111100 
    db 0b00000110
    db 0b11111110
    db 0b11111110
    db 0b11111110
    db 0b11111110
    db 0b11111100
    db 0b00000000
    ; 4: pill single
    db 0b01111100 
    db 0b11011110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b11111110
    db 0b01111100
    db 0b00000000
    ; 5: pill clear
    db 0b01111100 
    db 0b10000010
    db 0b10000010
    db 0b10000010
    db 0b10000010
    db 0b10000010
    db 0b01111100
    db 0b00000000
    ; 6: virus
    db 0b11000110
    db 0b00111000
    db 0b00010000
    db 0b10101010
    db 0b11111110
    db 0b10010010
    db 0b01111100
    db 0b00000000

colors:
    db PILL_YELLOW, PILL_RED, PILL_BLUE

times 510 - ($ - $$) db 0
dw 0xaa55
