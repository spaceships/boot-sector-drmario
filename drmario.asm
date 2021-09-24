    bits 16

%assign enable_load 1 ; load the rest of the game from the boot sector

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adjustable parameters ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
PILL_RED:       equ 0x27
PILL_YELLOW:    equ 0x2c
PILL_BLUE:      equ 0x36
BORDER_COLOR:   equ 0x6b
SPEED:          equ 4 ; higher = slower
PAUSE_LEN:      equ 3 ; pause = 131*x ms
VIRUS_THRESH:   equ 220 ; closer to 255 => less frequent
VIRUS_MAX:      equ 8 ; max num virii
NUM_ROWS:       equ 16 ; Original: 16
NUM_COLS:       equ 8 ; Original: 8

;;;;;;;;;;;;;;;;;;;;;;
;; fixed parameters ;;
;;;;;;;;;;;;;;;;;;;;;;
CELL_SIZE:    equ 8 ; size of square board cells in pixels
SPRITE_ROWS:  equ 7 ; vertical size of sprite in pixels
SPRITE_COLS:  equ 7 ; horizontal size of sprite in pixels
BOARD_HEIGHT: equ CELL_SIZE*NUM_ROWS
BOARD_WIDTH:  equ CELL_SIZE*NUM_COLS
BOARD_START:  equ (100-BOARD_HEIGHT/2)*320+(160-BOARD_WIDTH/2)
BOARD_END:    equ (100+BOARD_HEIGHT/2)*320+(160+BOARD_WIDTH/2)
COMMON_PIXEL: equ 5
CLEAR_PIXEL:  equ 643 ; 2 pixels down, 3 right
VIRUS_PIXEL:  equ 3*320+3
DIR_PIXEL:    equ 7 ; the top right pixel encodes the direction
DIR_UP:       equ 0xFF
DIR_DOWN:     equ 0xFD
DIR_LEFT:     equ 0xFC
DIR_RIGHT:    equ 0xFB

;;;;;;;;;;;;;;;;;;;;
;; game variables ;;
;;;;;;;;;;;;;;;;;;;;
base:           equ 0xfc80
pill_loc:       equ 0                   ; [word]
pill_color:     equ pill_loc + 2        ; [word]
pill_offset:    equ pill_color + 2      ; [word]
next_tick:      equ pill_offset + 2     ; [word]
rand:           equ next_tick + 2       ; [word]
num_virii:      equ rand + 2            ; [byte]
wait_flag:      equ num_virii + 1       ; [byte]

    org 0x7c00 ; Start address for the boot sector

%if enable_load
    ;;;;;;;;;;;;
    ;; loader ;;
    ;;;;;;;;;;;;
    mov ah,0x02 ; set interrupt to read disk sectors
    mov al,2 ; number of sectors to read
    mov ch,0 ; cylinder number
    mov cl,2 ; sector number
    mov dh,0 ; head number
    ; dl - drive number - is passed in by bios
    mov bx,0x7e00 ; es:bs - address to load to - 0x7c00 + 512
    int 0x13
    jmp game_start

    times 510 - ($ - $$) db 0
    dw 0xaa55 ; bios boot sector flag
%endif

game_start:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; initialization: 20 bytes ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov bp,base   ; set base address for global state
    mov ax,0x0013 ; set video mode vga 320x200x256
    int 0x10      ; call bios interrupt

    int 0x1a ; bios clock read, reusing above's ah=0
    mov [bp+rand],dx ; initialize rand

    mov ax,0xa000 ; set data & extra segments to video
    mov ds,ax
    mov es,ax
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; draw border: 29 bytes ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ; draw colored part
    mov al,BORDER_COLOR
    mov cx,bp ; 0xfc80>320*200
    xor di,di
    rep stosb
    ; draw black part
    xor al,al
    mov di,BOARD_START
    mov dx,BOARD_HEIGHT
.black:
    mov cl,BOARD_WIDTH ; ch is already 0
    rep stosb
    add di,320-BOARD_WIDTH
    dec dx
    jnz .black

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; place virii: 29 bytes ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov byte [bp+num_virii],al ; al=0
    mov bx,place_virii
    call each_cell

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; make a new pill: 26 bytes ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
pillnew:
    call rng ; al = color, ah = rand (3b)
    mov byte [bp+pill_color],al ; set first color
    call rng
    mov byte [bp+pill_color+1],al ; set second color
    mov di,BOARD_START+BOARD_WIDTH/2-CELL_SIZE ; initial pill loc
    mov bx,8 ; pillmove_no_clear needs bx set already
    mov [bp+pill_offset],bx ; initial rotation
    call pillmove.no_clear ; don't clear previous pill
    jnz game_start ; if occupied, restart game

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; initialization done, game loop follows ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
game_loop:
    mov di,[bp+pill_loc] ; pre-load pill location into di (3b)
    ;;;;;;;;;;;;;;;;;;;;
    ;; get user input ;;
    ;;;;;;;;;;;;;;;;;;;;
    ; BIOS keeps a buffer of key presses. Int 0x16/ah=0x01 checks if a key is available
    ; because int 0x16/ah=0x00 *waits* until a key is available. Unfortunately the buffer
    ; can be filled while we are waiting for pieces to fall in the bottom of the game
    ; loop. That allows the player to pre-move the pill before it even starts falling. The
    ; solution below is to empty the buffer every time.
.check_key_buf:
    mov ah,0x01 ; bios key available (2b)
    int 0x16 ; sets ZF=1 if no keystroke available (2b)
    jz .key_buf_empty ; (2b)
    mov ah,0x00 ; bios get keystroke from buffer
    int 0x16 ; ah=key scan code
    mov bl,ah ; store scan code in bl because both ints clobber ax 
    jmp .check_key_buf
.key_buf_empty:
    mov al,bl ; move to al since cmp al,x is 1 byte cheaper

.check_left:
    cmp al,0x4b ; left arrow
    jne .check_right
    sub di,8 ; move pill left 8 pixels
    call pillmove

.check_right:
    cmp al,0x4d ; right arrow
    jne .check_down
    add di,8 ; move pill right 8 pixels
    call pillmove

.check_down:
    cmp al,0x50 ; down arrow
    je pillfall

.check_a:
    xor cl,cl ; swap colors when going horiz
    cmp al,0x1e ; 'a'
    je pillrotate

.check_s:
    mov cl,8 ; swap colors when going virt
    cmp al,0x1f ; 's'
    jne clock
    ; fall through to pillrotate

; cl = 0 for rotate left, 8 for rotate right
pillrotate:
    call pillclear ; first clear the pill
    mov bx,8^(-8*320) ; toggle between +8 (horiz) and -8*320 (vert)
    xor bx,[bp+pill_offset] ; bx = proposed offset
    test byte [di+COMMON_PIXEL+bx],0xFF ; something there already?
    jnz .redraw ; if occupied, dont rotate
    mov [bp+pill_offset],bx ; actually change offset
    xor cl,bl ; set 8 or 0 depending on orientation
    ror word [bp+pill_color],cl ; possibly swap colors
.redraw:
    call pilldraw

    ;;;;;;;;;;;;;;;;
    ;; game clock ;;
    ;;;;;;;;;;;;;;;;
clock:
    mov ah,0x00
    int 0x1a ; bios clock read
    cmp dx,[bp+next_tick]
    jb game_loop
    add dx,SPEED
    mov [bp+next_tick],dx
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; make the pill fall ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;
pillfall:
    add di,8*320 ; proposed loc: down 1 row
    call pillmove ; pillmove sets ZF when it is successful
    jz game_loop ; no obstructions, continue game loop
    ; otherwise, done falling this pill
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; check for clears (14b) ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
clearcheck:
    mov bx,matchcheck ; check for a match, change it to clear
    call each_cell ; run matchcheck on every cell
    shr byte [bp+wait_flag],1 ; check & clear wait_flag
    jnc .cont ; if it is not set, continue game
    call pause
    ;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; remove cleared pills ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov bx,remove_cleared
    call each_cell
    ;;;;;;;;;;;;;;;;;;;;;
    ;; make stuff fall ;;
    ;;;;;;;;;;;;;;;;;;;;;
.fall:
    mov bx,fall_stuff
    call each_cell
    shr byte [bp+wait_flag],1 ; check & clear wait_flag
    jnc .nextcheck ; if it is not set, nothing fell, continue game
    call pause ; otherwise pause 
    jmp .fall
.nextcheck:
    jmp clearcheck ; ... keep running this phase
.cont:
    cmp byte [bp+num_virii],0
    ja pillnew
.halt:
    hlt
    jmp .halt
    ;;;;;;;;;;;;;;;;;;;;;;
    ;; end of game loop ;;
    ;;;;;;;;;;;;;;;;;;;;;;

; wait for a bit
pause:
    mov cx,PAUSE_LEN
    mov ah,0x86
    int 0x15
    ret

; di: proposed location
; sets ZF=1 if nothing is in the way
; sets pill_loc=di and pill_offset=bx if possible
pillmove:
    push di
    call pillclear ; clear current pill, sets bx=offset
    pop di
.no_clear:
    mov al,[di+COMMON_PIXEL]
    or al,[di+COMMON_PIXEL+bx] ; is either cell occupied?
    jnz .restore ; if yes, restore pill and return
    mov [bp+pill_loc],di ; ok we're clear, move pill
.restore:
    pushf ; save flags from above (used by pillfall)
    call pilldraw ; (re)draw pill
    popf ; restore flags from above
.done:
    ret

; di: top left pixel
; clobbers ax,dx,si,di
pillclear:
    xor ax,ax
    call pilldraw.common ; sets bx=offset, di=loc + offset
    mov byte [di+DIR_PIXEL],0 ; clear offset dir (set by pilldraw)
    sub di,bx ; subtract offset from di
    mov byte [di+DIR_PIXEL],0 ; clear primary dir
    ret

; set bx to offset, di to loc + offset, clobbers ax, si
; sets DIR_PIXELs
pilldraw:
    mov ax,[bp+pill_color]
.common:
    mov di,[bp+pill_loc]
    mov bx,[bp+pill_offset]
    mov si,sprite_bottom
    mov dx,DIR_DOWN*256+DIR_UP
    test bx,bx
    js .draw
    mov si,sprite_left
    mov dx,DIR_LEFT*256+DIR_RIGHT
.draw:
    call draw_sprite
    mov [di+DIR_PIXEL],dl
    ; `draw_sprite` leaves `si` set to the start of the next sprite
    mov al,ah ; swap colors
    add di,bx
    call draw_sprite
    mov [di+DIR_PIXEL],dh
    ret

; di: top left pixel
; si: start of sprite
; al: color
draw_sprite:
    pusha
    mov byte [di+DIR_PIXEL],0 ; always clear dir pixel
    mov ah,al ; save color in ah
    mov dx,SPRITE_ROWS
.row:
    cs lodsb ; load byte of the sprite into al, advance si
    mov bl,al ; save it in bl
.row_again:
    mov cx,SPRITE_COLS ; ds_col row runs 7 times
.col:
    xor al,al ; set al = 0 = black
    rol bl,1 ; rotate bl left by 1, sets CF
    jnc .print ; if we just shifted off a 0, print black pixel
    mov al,ah ; otherwise get the color
.print:
    stosb ; [di++] = al, print color to current pixel loc
    loop .col
    add di,320-7 ; increment di: +1 row, -8 cols
    ; The `ds_col` loop has run 7 times, rotating the original `bl` from
    ; `0xXXXXXXXR` 7 places to produce `0xRXXXXXXX`.  Shifting by 1 more bit
    ; copies R into the carry flag and sets `bl` to `0xXXXXXXX0`, so we'll draw
    ; the same row (if R is set) but won't repeat again afterward.
    shl bl,1 ; shift sprite row over by 1, setting CF, zeroing repeat bit
    dec dx ; decrement row counter, preserving the carry flag
    jc .row_again ; if the last bit of bl was 1, do the row again
    jnz .row ; if dx!=0, do another row
    ; Preserve the final `si`, leaving it set to the start of the next sprite.
    ; `pilldraw` uses this to draw the second half of the two-part pill.
    mov bp,sp
    mov [bp+2],si
    popa
.ret:
    ret

; potentially place a virus at di, if there aren't too many already and
; the virus wins the dice roll
; -intended to be used with each_cell
place_virii:
    call rng
    cmp ah,VIRUS_THRESH
    jb draw_sprite.ret ; return
    cmp byte [bp+num_virii],VIRUS_MAX
    je draw_sprite.ret ; return if num_virii has reached the limit
    inc byte [bp+num_virii] ; increment virii counter
    mov si,sprite_virus ; draw virus sprite
    jmp draw_sprite

; check the column up to 4 below and 4 to the right
; changes matches into sprite_clear
; -intended to be used with each_cell
matchcheck:
    mov bx,8
    call check4
    mov bx,8*320
    ; fall through to check4

; checking 4 in row starting at di, offset by bx
; -helper for matchcheck
check4:
    push di ; di can be mangled arbitrarily
    mov al,[di+COMMON_PIXEL] ; get first sprite color
    test al,al
    jz .done ; ignore black cells
    mov cx,3
.check:
    add di,bx ; add offset
    cmp al,[di+COMMON_PIXEL] ; compare 3 offsets by bx
    jne .done ; if not equal, return
    loop .check
    mov cx,4 ; clear all 4 sprites
    ; if it is a virus, dec num_virii
    cmp byte [di+VIRUS_PIXEL],0 ; virii don't have pixel set here
    jne .clear; if it not 0, then its not a virus
    dec byte [bp+num_virii]
.clear:
    mov dl,[di+DIR_PIXEL] ; save direction to dl
    mov si,sprite_clear ; change to clear sprite
    call draw_sprite ; al=color is set from above
    ; change any pill part neighbors to sprite_single
    call dir_to_offset ; uses dl as arg, sets dx=offset
    jne .cont ; continue if no DIR_PIXEL
    mov si,sprite_single
    add di,dx ; add the direction offset to di
    push ax ; save the color we are clearing
    mov al,[di+COMMON_PIXEL] ; get the color of the sprite to be replaced
    call draw_sprite ; redraw it as sprite_single (sets dir=0)
    pop ax ; restore clearing color
    sub di,dx ; restore di
.cont:
    sub di,bx ; subtract offset
    loop .clear
    mov byte [bp+wait_flag],1 ; set wait flag
.done:
    pop di
.ret:
    ret

; convert dl=direction to dx=offset (40 bytes)
dir_to_offset:
    cmp dl,DIR_UP
    jne .test_down
    mov dx,-8*320
    ret
.test_down:
    cmp dl,DIR_DOWN
    jne .test_left
    mov dx,8*320
    ret
.test_left:
    cmp dl,DIR_LEFT
    jne .test_right
    mov dx,-8
    ret
.test_right:
    cmp dl,DIR_RIGHT
    mov dx,8
    jne .none
    mov dl,0 ; doesn't set flags
.none:
    ret


; remove any cleared pill piece in the cell
; di: top left corner
; intended to be used with each_cell
remove_cleared:
    test byte [di+CLEAR_PIXEL],0xFF
    jnz check4.ret ; near jump to return statement (2b)
    xor al,al
    jmp draw_sprite

; make pieces fall that can fall
; intended to be used with each_cell
fall_stuff:
    cmp byte [di+VIRUS_PIXEL],0 ; virii don't have pixel set here
    je check4.ret ; if it is 0, then its a virus so don't make it fall

    cmp byte [di+8*320+COMMON_PIXEL],0 ; is there something directly below?
    jnz check4.ret ; if not zero, there is something there, bail

    mov al,[di+DIR_PIXEL] ; set al = direction
    mov si,di ; set source for move sprite
    add di,8*320 ; set target to be the sprite below

    cmp al,DIR_LEFT ; is the direction LEFT?
    je .fall_left_right
    cmp al,DIR_RIGHT ; is the direction RIGHT?
    jne .success ; nope, fall

.fall_left_right:
    ; ok it is left or right, get offset
    mov dl,al
    call dir_to_offset ; dx=offset
    mov bx,dx
    cmp byte [di+bx+COMMON_PIXEL],0 ; something below to the left/right?
    jnz check4.ret ; if there is something there, bail
    add si,dx ; otherwise copy the left/right sprite down
    add di,dx
    call move_sprite
    sub di,dx ; restore di
    sub si,dx ; restore si

.success:
    mov byte [bp+wait_flag],1 ; set wait flag
    ; fall through to move_sprite to move primary sprite

; move sprite starting at si to start at di
move_sprite:
    mov bx,8
.outer:
    mov cx,8 
.inner:
    movsb ; [di++] = [si++]
    mov byte [si-1],0
    loop .inner
    add di,312 ; increment di to next row of pixels
    add si,312 ; increment si to next row of pixels
    dec bx
    jnz .outer
    ret

; call a function in bx with di set to start of each cell on the board
each_cell:
    ; start at bottom right cell and work back.
    ; this is for place_virii so it can place most of them at the bottom.
    mov di,BOARD_END-CELL_SIZE*320-CELL_SIZE
.outer:
    mov cx,NUM_COLS
.inner:
    pusha
    call bx
    popa
    sub di,CELL_SIZE
    loop .inner
    ; if cx is 0, then subtract row
    sub di,CELL_SIZE*320-BOARD_WIDTH
    cmp di,BOARD_START
    ja .outer
    ret

; put a random color from colors array into al
; put a random byte into ah
rng:
    ; This implements an LFSR with two taps: the sequence of bits satisfies
    ; the recurrence R(n) = R(n-7) + R(n-15).
    ; As x^15 + x^7 + 1 is a primitive polynomial in GF(2), this sequence
    ; attains the maximal period of 2^15-1 = 32767.
    ; Each call to this routine advances the LFSR by 7 bits, shifting in
    ; new bits on the left.
    mov ax,[bp+rand]
    mov bx,ax
    xor bl,bh
    shr ax,7
    mov ah,bl
    mov [bp+rand],ax ; save new seed
    ; ah is set: set al to be a random color from the colors array
    and al,3 ; mask off bottom 2 bits of al
    jz rng   ; make sure at least one bit is set
    mov bx,colors-1
    cs xlat  ; al = [colors + al]
    ret

; All sprites consist of 7 rows of 7 pixels.
; The format of each row is 0xXXXXXXXR.  One X bit per column, indicating
; whether to draw the current color (1) or black (0) for that column.  The
; final R bit can be set to 1 to repeat the row twice.
sprite_bottom:
    db 0b10111111
    db 0b10111111
    db 0b10111110
    db 0b11111110
    db 0b01111100
sprite_top:
    db 0b01111100
    db 0b11011110
    db 0b10111111
    db 0b10111111
    db 0b11111110
sprite_left:
    db 0b01111110
    db 0b11000010
    db 0b10111110
    db 0b11111111
    db 0b11111110
    db 0b01111110
sprite_right:
    db 0b11111100
    db 0b00000110
    db 0b11111111
    db 0b11111111
    db 0b11111100
sprite_single:
    db 0b01111100
    db 0b11011110
    db 0b10111111
    db 0b10111110
    db 0b11111110
    db 0b01111100
sprite_clear:
    db 0b01111100
    db 0b10000011
    db 0b10000011
    db 0b10000010
    db 0b01111100
sprite_virus:
    db 0b11000110
    db 0b00111000
    db 0b00010000
    db 0b10101010
    db 0b11111110
    db 0b10010010
    db 0b01111100

colors:
    db PILL_YELLOW, PILL_RED, PILL_BLUE

%if !enable_load
    times 510 - ($ - $$) db 0
    dw 0xaa55 ; bios boot sector flag
%endif
