; TODO:
;   * check for cures
;   * random color 
;   * control pills L+R
;   * control pills rotation
;   * random viruses!

    bits 16

base:       equ 0xfc80
tmp_byte:   equ base
tmp_word:   equ tmp_byte + 1
next_tick:  equ tmp_word + 2    ; [word] last time we got from int
rand:       equ next_tick + 2    ; [word] rng seed
pill_falling:   equ rand + 2    ; [byte] is a pill falling
cur_pill_loc:   equ pill_falling + 1 ; [word] loc of current pill
cur_pill_rot:   equ cur_pill_loc + 2 ; [byte] rotation of pill:
                                     ; 0: 0, 1: 90, 2: 180, 3: 270
board:      equ cur_pill_rot + 1 ; 8x16x2 low: type, high: color

BLACK:          equ 0x00
PILL_RED:       equ 0x27
PILL_YELLOW:    equ 0x2c
PILL_BLUE:      equ 0x36
BORDER_COLOR:   equ 0x6a
SPEED:          equ 4

BOARD_X:    equ 16
BOARD_Y:    equ 5

start:
    mov ax,0x0013   ; set video mode vga 320x200x256
    int 0x10        ; call bios interrupt

    mov ax,0xa000   ; set data & extra segments to video
    mov ds,ax
    mov es,ax

    call draw_outline

    ; mov byte [pill_falling],0

game_loop:
    ; get the player's input
    mov ah,0x01 ; bios key available
    int 0x16
    mov ah,0x00
    je gl_no_key
    int 0x16
    mov bx,[cur_pill_loc]
    mov cx,[board+bx]
gl_no_key:
    cmp ah,0x4b ; left arrow
    jne gl_p0
    cmp byte [board+bx-2],0
    jnz gl_nokey
    mov [board+bx-2],cx
    mov word [board+bx],0
    jmp gl_redraw
gl_p0:
;     cmp ah,0x4d ; right arrow
;     jne gl_p1
;     ;; TODO move right
; gl_p1:
;     cmp ah,0x50 ; down arrow
;     jne gl_nokey
;     call pillfall 
gl_redraw:
    call draw_board
gl_nokey:
    
    ; clock stuff follows
    mov ah,0x00
    int 0x1a    ; bios clock read
    cmp dx,[next_tick]
    jb game_loop
    add dx,SPEED
    mov [next_tick],dx
    mov [rand],dx

gl_skip_clock:
    cmp byte [pill_falling],0    ; see if a pill is currently falling
    jne gl_pill_falling
    call new_pill                ; no pill falling, make a new pill
gl_pill_falling:
    call pillfall  
    call draw_board
    jmp game_loop

; Make a pill fall if it can
pillfall:
    cmp byte [pill_falling],0    ; see if a pill is currently falling
    je pf_done
    ; Checking whether we are done
    cmp word [cur_pill_loc],(16*8*2-15) ; are we at the bottom row?
    jl pf_keep_checking         ; if y is not 0, keep going
pf_stop_falling:
    mov byte [pill_falling],0   ; clear pill falling flag
    jmp pf_done
pf_keep_checking:
    cmp byte [board+bx+16],0    ; is there something below?
    jne pf_stop_falling         ; if there is, stop falling
    ; If pill is horizontal, check if right is filled too
    and byte [cur_pill_rot],1   ; is cur_pill_rot odd? 
    jnz pf_fall                 ; if odd, fall
    cmp byte [board+bx+18],0    ; otherwise is there something there?
    jne pf_stop_falling         ; if there is, stop falling
    mov word ax,[board+bx+2]    ; fall right side
    xchg ax,[board+bx+18]       ; copy right side down
    mov word [board+bx+2],0     ; zero out old right side
pf_fall:
    mov word ax,[board+bx]      ; fall left side
    xchg [board+bx+16],ax   ; copy it down
    mov word [board+bx],0       ; zero out old one
    add word [cur_pill_loc],16  ; update pill location
pf_done:
    ret

; randomize rand variable
; mangles ax,dx
rng:
    mov ax,8086
    mul word [rand]
    add ax,8086
    mov [rand],ax
    ret

; put a random color from colors array into al
; mangles ax,bx,dx
rand_color:
    call rng    ; sets ax
    xor dx,dx
    mov bx,3
    div bx
    mov al,dl   ; al = remainder
    mov bx,colors
    cs xlat     ; al = [colors + al]
    ret

new_pill:
    mov byte [board+6],0x03
    call rand_color
    mov byte [board+7],al
    mov byte [board+8],0x04
    call rand_color
    mov byte [board+9],al
    mov word [cur_pill_loc],6
    mov byte [cur_pill_rot],0
    mov byte [pill_falling],1 
    ret

draw_board:
    mov cx,0 ; row
db_row:
    mov dx,0 ; col
db_col:
    mov al,cl       ; al = row
    mov bl,16       ; bl = 16
    mul bl          ; ax = 16 * row (don't clobber dx)
    push ax
    mov bl,dl       ; bx = col
    mov al,2
    mul bl
    pop bx
    add bx,ax       ; bx = 16*row + col*2
    mov bx,[board+bx]
    mov al,dl       ; al = col
    add al,BOARD_X  ; al = col + BOARD_X
    mov ah,cl       ; ah = row
    add ah,BOARD_Y  ; ah = row + BOARD_Y
    call draw_sprite
    inc dx 
    cmp dx,8
    jne db_col
    inc cx
    cmp cx,16
    jne db_row 
    ret

draw_outline:
    mov bx,BORDER_COLOR*256
    mov ah,BOARD_Y-2
do_y_loop:
    mov al,BOARD_X-1
    cs mov cx,[border_mask+si]
do_x_loop:
    mov di,cx
    and di,1
    jz do_skip_draw
    call draw_sprite
do_skip_draw:
    inc al
    shr cx,1
    jnz do_x_loop
    cmp ah,BOARD_Y
    jl do_increment_i
    cmp ah,BOARD_Y+15
    jl do_not_increment_i
do_increment_i:
    ; one byte smaller than add si,2
    inc si
    inc si
do_not_increment_i:
    inc ah
    cmp ah,BOARD_Y+16
    jle do_y_loop
    ret

; al: x
; ah: y
; bl: sprite index
; bh: color
draw_sprite:
    push ax
    push bx
    push cx     ; clobbered non-argument registers
    push dx
    push si
    push di
    mov [tmp_byte],bh   ; tmp_byte = color
    mov [tmp_word],ax   ; save ax (x,y) before clobbering ax
    ; compute the base index of the sprite
    mov al,8
    mul bl
    add ax,sprites
    mov si,ax       ; si now contains the start of the sprite
    ; mov cl,[tmp_word]   ; ax = (x,y)
    ; top left PIXEL is 8*y * 320 + 8*x
    ; the following sets di to be the top left PIXEL
    mov ax,(8*320)
    mul word [tmp_word+1] ; ax = y * 320 * 8
    mov bx,ax       ; save at bx
    mov al,8
    mul byte [tmp_word] ; ax = 8*x
    add ax,bx       ; ax = 8*y * 320 + 8*x
    mov di,ax       ; di contains top left PIXEL now
    ; Iterate over each pixel in the sprite
    mov dl,0        ; dl = row
ds_row:
    mov dh,0        ; dh = col
    cs lodsb        ; load the whole byte of the sprite into al
    mov cl,al       ; save it in cl
    mov bl,0x80     ; mask
ds_col:
    test cl,bl      ; is the bit set in the bitmask?
    jz ds_black     ; if it isn't, print black pixel
    mov al,[tmp_byte]   ; get the saved color
    jmp ds_print    ; skip the following line
ds_black:
    mov al,BLACK
ds_print:
    stosb           ; print color to current pixel loc
    inc dh          ; col++
    shr bl,1        ; move mask over by 1
    cmp dh,8        ; are we done with column yet?
    jne ds_col      ; if dh != 8, jump to ds_col
    add di,312      ; increment di
    inc dl          ; row++
    cmp dl,8 
    jne ds_row      ; if row != 8, jump to ds_row
    ; function finished, cleanup follows
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    ret

sprites:
    ; 0: border
    db 0b11111111
    db 0b11111111
    db 0b11111111
    db 0b11111111
    db 0b11111111
    db 0b11111111
    db 0b11111111
    db 0b11111111
    ; 1: pill top
    db 0b01111100 
    db 0b11011110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b11111110
    db 0b00000000
    ; 2: pill bottom
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b11111110
    db 0b01111100
    db 0b00000000
    ; 3: pill left
    db 0b01111110 
    db 0b11000010
    db 0b10111110
    db 0b11111110
    db 0b11111110
    db 0b11111110
    db 0b01111110
    db 0b00000000
    ; 4: pill right
    db 0b11111100 
    db 0b00000110
    db 0b11111110
    db 0b11111110
    db 0b11111110
    db 0b11111110
    db 0b11111100
    db 0b00000000
    ; 5: pill single
    db 0b01111100 
    db 0b11011110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b11111110
    db 0b01111100
    db 0b00000000
    ; 6: pill clear
    db 0b01111100 
    db 0b10000010
    db 0b10000010
    db 0b10000010
    db 0b10000010
    db 0b10000010
    db 0b01111100
    db 0b00000000

border_mask:
    dw 0b0000000001001000
    dw 0b0000001111001111
    dw 0b0000001000000001
    dw 0b0000001111111111

colors:
    db PILL_YELLOW, PILL_RED, PILL_BLUE

times 510 - ($ - $$) db 0
dw 0xAA55
