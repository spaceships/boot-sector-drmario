    bits 16

base:           equ 0xfc80
sprite_color:   equ base                ; [byte]
cur_pill_loc:   equ sprite_color + 1    ; [word]
pill_offset:    equ cur_pill_loc + 2    ; [word]
next_tick:      equ pill_offset + 2     ; [word]
rand:           equ next_tick + 2       ; [word]

BLACK:          equ 0xFF
PILL_RED:       equ 0x27
PILL_YELLOW:    equ 0x2c
PILL_BLUE:      equ 0x36
BORDER_COLOR:   equ 0x6a
SPEED:          equ 4
VIRUS_COUNT:    equ 4

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

    ;;;;; place virii
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
    ;;; done placing virii

    call draw_outline
    call new_pill

game_loop:
    ;;; User input
    ; precompute first offset for R and second offset for LR
    mov bx,16       ; R: check 16 pixels over
    xor cx,cx       ; LR/horiz: only check 1 place
    cmp word [pill_offset],8 ; horizontal?
    je gl_offset_ok
    shr bx,1        ; vertical, div bx by 2
    mov cx,-8*320   ; second checking offset is above
gl_offset_ok:

    ; get the player's input
    mov ah,0x01 ; bios key available
    int 0x16
    mov ah,0x00
    je gl_check_left
    int 0x16
gl_check_left:
    cmp ah,0x4b ; left arrow
    jne gl_check_right
    mov ax,-8
    mov bx,ax
    call pillmove
gl_check_right:
    cmp ah,0x4d ; right arrow
    jne gl_check_down
    mov ax,8
    call pillmove
gl_check_down:
    cmp ah,0x50 ; down arrow
    jne gl_check_a
    call pillfall 
gl_check_a:
    cmp ah,0x1e ; 'a'
    jne gl_clock
    call pillrot
    ; cmp ah,0x1f ; 's'

gl_clock:
    ; clock stuff
    mov ah,0x00
    int 0x1a    ; bios clock read
    cmp dx,[next_tick]
    jb game_loop
    add dx,SPEED
    mov [next_tick],dx

    call pillfall
    jmp game_loop

pillrot:
    cmp word [pill_offset],8
    je pr_horiz
    ret
pr_horiz:
    mov word [pill_offset],-8*320
    mov si,[cur_pill_loc] 
    mov di,si
    sub di,8*320
    add si,8
    jmp move_sprite

pillfall:
    mov ax,8*320
    mov bx,ax
    mov cx,8
    cmp byte [pill_offset],8
    je pf_call
    xor cx,cx
pf_call:
    call pillmove
    jz pf_done
    call new_pill
pf_done:
    ret

; ax: moving offset
; bx: checking offset 1
; cx: checking offset 2 (in addition to bx)
pillmove:
    mov dx,[cur_pill_loc]
    mov di,dx
    add di,bx ; add in the testing offset 1
pm_test:
    call occ
    jnz pm_done
    add di,cx
    call occ
    jnz pm_done
pm_move:
    pushf
    xor di,di
    mov si,dx
    call move_sprite        ; save left part
    mov si,dx               ; move right/up part of pill
    add si,[pill_offset]
    mov di,si
    add di,ax
    call move_sprite
    xor si,si               ; restore left part
    mov di,dx
    add di,ax
    call move_sprite
    add word [cur_pill_loc],ax
    popf
pm_done:
    ret

; occupied(di) ZF=0 if occupied
occ:
    test byte [di],0xFF ; top left corner
    jnz occ_done
    test byte [di+7*320+7],0xFF ; bottom right corner
occ_done:
    ret

; si: source
; di: target
; clobbers cx, si, di
move_sprite:
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
    mov word [pill_offset],8
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
