; TODO:
;   * check for cures
;   * random color 
;   * control pills L+R
;   * control pills rotation
;   * random viruses!

    bits 16

base:       equ 0xfc80
old_time:   equ base    ; [word] last time we got from int
pill_falling:   equ base + 2 ; [byte] is a pill falling
cur_pill_loc:   equ base + 3 ; [word] x,y of current pill
cur_pill_rot:   equ base + 5 ; [byte] rotation of pill:
                             ; 0: 0, 1: 90, 2: 180, 3: 270
board:      equ base + 6 ; 8x16x2 low: type, high: color

BLACK:          equ 0x00
PILL_RED:       equ 0x27
PILL_YELLOW:    equ 0x2c
PILL_BLUE:      equ 0x3c
BORDER_COLOR:   equ 0x6a

BOARD_X:    equ 16
BOARD_Y:    equ 5

start:
    mov ax,0x0013   ; set video mode vga 320x200x256
    int 0x10        ; call bios interrupt

    mov ax,0xa000   ; set data & extra segments to video
    mov ds,ax
    mov es,ax

    call draw_outline

    mov byte [pill_falling],0

game_loop:
    mov ah,0x00
    int 0x1a    ; bios clock read
    cmp dx,[old_time]
    je game_loop
    mov [old_time],dx

    cmp byte [pill_falling],0    ; see if a pill is currently falling
    jne gl_pill_falling
    call new_pill ; no pill falling, make a new pill
gl_pill_falling:
    call pillfall  
    call draw_board
    jmp game_loop

; Make a pill fall if it can
pillfall:
    ; Checking whether we are done
    mov ax,[cur_pill_loc]       ; al = x, ah = y
    cmp ah,15
    jl pf_keep_checking        ; if y is not 0, keep going
pf_stop_falling:
    mov byte [pill_falling],0   ; clear pill falling flag
    jmp pf_done
    ; Checking whether there is another pill below
pf_keep_checking:
    ; Get current board location
    mov al,ah   ; al = y
    mov bl,16   ; mult by 16
    mul bl      ; ax = 16*y
    mov cx,ax   ; cx = 16*y
    mov ax,[cur_pill_loc] ; al = x
    mov bl,2
    mul bl      ; ax = 2*x
    add ax,cx   ; ax = 16*y+2*x
    mov bx,ax   ; bx = 16*y+2*x
    cmp byte [board+bx+16],0    ; is there something below?
    jne pf_stop_falling         ; if there is, stop falling
    ; If pill is horizontal, check if right is filled too
    and byte [cur_pill_rot],1   ; is cur_pill_rot odd? 
    jnz pf_fall                 ; if odd, fall
    cmp byte [board+bx+18],0    ; otherwise is there something there?
    jne pf_stop_falling         ; if there is, stop falling
    mov word ax,[board+bx+2]    ; fall right side
    mov word [board+bx+18],ax   ; copy right side down
    mov word [board+bx+2],0     ; zero out old right side
pf_fall:
    mov word ax,[board+bx]      ; fall left side
    mov word [board+bx+16],ax   ; copy it down
    mov word [board+bx],0       ; zero out old one
    add word [cur_pill_loc],0x0100  ; update pill location

pf_done:
    ret

new_pill:
    ; TODO: randomize color
    mov byte [board+6],0x03
    mov byte [board+7],PILL_RED
    mov byte [board+8],0x04
    mov byte [board+9],PILL_BLUE
    mov word [cur_pill_loc],0x0003
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
    mov byte al,[board+bx]   ; bl = TYPE
    mov byte ah,[board+bx+1] ; bh = COLOR
    mov bx,ax
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
    mov bl,0
    mov bh,BORDER_COLOR
    mov al,BOARD_X-1
    mov ah,BOARD_Y-2
    mov cx,18
db_left_col:
    inc ah
    call draw_sprite
    loop db_left_col
    mov cx,9
db_bottom_row:
    call draw_sprite
    inc al
    loop db_bottom_row
    mov cx,18
db_right_col:
    call draw_sprite
    dec ah
    loop db_right_col
    add ah,1
    mov cx,3
db_right_top:
    dec al
    call draw_sprite
    loop db_right_top
    dec ah
    call draw_sprite
    sub al,3
    call draw_sprite
    add ah,1
    call draw_sprite
    mov cx,2
db_left_top:
    dec al
    call draw_sprite
    loop db_left_top
    
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
    xor ch,ch   ; save the color before clobbering bx
    mov cl,bh   ; cl = color, will be saved on stack later
    push ax     ; save ax (x,y) before clobbering ax
    ; compute the base index of the sprite
    mov bh,0        ; bx = sprite index
    mov al,8
    mul bl
    add ax,sprites
    mov si,ax   ; si contains the start of the sprite
    pop ax      ; pop ax for the computation below
    push cx     ; save color for later
    ; top left PIXEL is 8*y * 320 + 8*x
    ; the following sets di to be the top left PIXEL
    mov cl,al       ; cl = x
    xor bh,bh
    mov bl,ah       ; bx = y
    mov ax,(8*320)
    mul bx          ; ax = y * 320
    mov bx,ax       ; save at bx
    mov al,8
    mul cl          ; ax = 8*x
    add ax,bx       ; ax = 8*y * 320 + 8*x
    mov di,ax       ; di contains top left PIXEL now
    ; Iterate over each pixel in the sprite
    xor dx,dx
    mov dl,0        ; dl = row
ds_row:
    mov dh,0        ; dh = col
    cs lodsb        ; load the whole byte of the sprite into al
    mov cl,al       ; store the sprite row in cl
    mov bl,0x80     ; mask
ds_col:
    test cl,bl      ; is the bit set in the bitmask?
    jz ds_black     ; if it isn't, print black pixel
    pop ax          ; put the color in al to print
    push ax         ; also, save it for later
    jmp ds_print    ; skip the following line
ds_black:
    mov al,BLACK
ds_print:
    stosb           ; print color to current pixel loc
    inc dh          ; col++
    shr bl,1        ; move mask over by 1
    cmp dh,8        ; are we done with column yet?
    jne ds_col      ; if dh != 8, jump to ds_col
    mov ax,312      ; offset to the start of the next row
    add di,ax       ; increment di
    inc dl          ; row++
    cmp dl,8 
    jne ds_row      ; if row != 8, jump to ds_row
    ; function finished, cleanup follows
    pop ax          ; clear stack - we used it to store color
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
    db 0b00000010
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

%include "end.asm"
