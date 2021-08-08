    bits 16

; conditional compilation of features
%assign enable_virii 1
%assign enable_rng 1
%assign enable_fallstuff 0

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

start:
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
    mov cx,320*200
    xor di,di
    rep stosb
    ; draw black part
    xor al,al
    mov di,BOARD_START
    mov dx,BOARD_HEIGHT
db_black:
    mov cx,BOARD_WIDTH
    rep stosb
    add di,320-BOARD_WIDTH
    dec dx
    jnz db_black

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; place virii: 29 bytes ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;
%if enable_virii
    mov byte [bp+num_virii],VIRUS_MAX
    mov bx,place_virii
    call each_cell
%endif

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
    call pillmove_no_clear ; don't clear previous pill
    jnz start ; if occupied, restart game

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; initialization done, game loop follows ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
game_loop:
    mov di,[bp+pill_loc] ; pre-load pill location into di (3b)
    ;;;;;;;;;;;;;;;;;;;;
    ;; get user input ;;
    ;;;;;;;;;;;;;;;;;;;;
    ; get the actual key, if it is pressed
    mov ah,0x01 ; bios key available (2b)
    int 0x16 ; sets ZF=1 if no keystroke available (2b)
    je gl_check_left ; (2b)
    mov ah,0x00 ; bios get keystroke
    int 0x16    ; ah=key scan code
    mov al,ah   ; move to al since cmp al,x is 1 byte cheaper

gl_check_left:
    cmp al,0x4b ; left arrow
    jne gl_check_right
    sub di,8 ; move pill left 8 pixels
    call pillmove

gl_check_right:
    cmp al,0x4d ; right arrow
    jne gl_check_down
    add di,8 ; move pill right 8 pixels
    call pillmove

gl_check_down:
    cmp al,0x50 ; down arrow
    je pillfall

gl_check_a:
    xor cl,cl ; swap colors when going horiz
    cmp al,0x1e ; 'a'
    je gl_rotate_pill

gl_check_s:
    mov cl,8 ; swap colors when going virt
    cmp al,0x1f ; 's'
    jne gl_clock

; cl = 0 for rotate left, 8 for rotate right
gl_rotate_pill:
    call pillclear
    mov bx,8^(-8*320)
    xor bx,[bp+pill_offset] ; toggle between +8 (horiz) and -8*320 (vert)
    test byte [di+COMMON_PIXEL+bx],0xFF ; something there already?
    jnz gl_rotate_redraw ; if occupied, dont rotate
    mov [bp+pill_offset],bx ; actually change offset
    xor cl,bl ; set 8 or 0 depending on orientation
    ror word [bp+pill_color],cl ; possibly swap colors
gl_rotate_redraw:
    call pilldraw

    ;;;;;;;;;;;;;;;;
    ;; game clock ;;
    ;;;;;;;;;;;;;;;;
gl_clock:
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
    add di,8*320 ; move down 1 row
    call pillmove ; pillmove sets ZF when it is successful
    jz game_loop ; no obstructions, continue game loop
    ; otherwise, done falling this pill
    ;;;;;;;;;;;;;;;;;;;;;;
    ;; check for clears ;;
    ;;;;;;;;;;;;;;;;;;;;;;
clearcheck:
    mov bx,matchcheck ; check for a match, change it to clear
    call each_cell ; run matchcheck on every cell
    shr byte [bp+wait_flag],1 ; check & clear wait_flag
    jnc cc_nopause1 ; if it is not set, no wait
    call pause
cc_nopause1:
    ;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; remove cleared pills ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;
    mov bx,remove_cleared
    call each_cell
    ;;;;;;;;;;;;;;;;;;;;;
    ;; make stuff fall ;;
    ;;;;;;;;;;;;;;;;;;;;;
%if enable_fallstuff
    mov bx,fall_stuff
    call each_cell
    shr byte [bp+wait_flag],1 ; check & clear wait_flag
    jnc pillnew ; if it is not set, continue game
    call pause
    jmp clearcheck ; ... keep running this phase
%else
    jmp pillnew
%endif
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
pillmove_no_clear:
    mov al,[di+COMMON_PIXEL]
    or al,[di+COMMON_PIXEL+bx] ; is either cell occupied?
    jnz pm_restore ; if yes, restore pill and return
    mov [bp+pill_loc],di ; ok we're clear, move pill
pm_restore:
    pushf
    call pilldraw ; (re)draw pill
    popf
pm_done:
    ret

; di: top left pixel
; clobbers ax,dx,si,di
pillclear:
    xor ax,ax ; set color to none
    jmp pd_common

; set bx to offset, di to loc + offset, clobbers ax, si
pilldraw:
    mov ax,[bp+pill_color]
pd_common:
    mov di,[bp+pill_loc]
    mov bx,[bp+pill_offset]
    mov si,sprite_bottom
    test bx,bx
    js pd_bottom
    add si,10 ; horizontal: move si ahead to sprite_left
pd_bottom:
    call draw_sprite
    ; `draw_sprite` leaves `si` set to the start of the next sprite
    mov al,ah ; swap colors
    add di,bx
    ; fall through to draw_sprite

; di: top left pixel
; si: start of sprite
; al: color
draw_sprite:
    pusha
    mov ah,al ; save color in ah
    mov dx,SPRITE_ROWS
ds_row:
    cs lodsb ; load byte of the sprite into al, advance si
    mov bl,al ; save it in bl
ds_row_again:
    mov cx,SPRITE_COLS ; ds_col row runs 7 times
ds_col:
    xor al,al ; set al = 0 = black
    rol bl,1 ; rotate bl left by 1, sets CF
    jnc ds_print ; if we just shifted off a 0, print black pixel
    mov al,ah ; otherwise get the color
ds_print:
    stosb ; [di++] = al, print color to current pixel loc
    loop ds_col
    add di,320-7 ; increment di: +1 row, -8 cols
    ; The `ds_col` loop has run 7 times, rotating the original `bl` from
    ; `0xXXXXXXXR` 7 places to produce `0xRXXXXXXX`.  Shifting by 1 more bit
    ; copies R into the carry flag and sets `bl` to `0xXXXXXXX0`, so we'll draw
    ; the same row (if R is set) but won't repeat again afterward.
    shl bl,1 ; shift sprite row over by 1, setting CF, zeroing repeat bit
    dec dx ; decrement row counter, preserving the carry flag
    jc ds_row_again ; if the last bit of bl was 1, do the row again
    jnz ds_row ; if dx!=0, do another row
ds_end:
    ; Preserve the final `si`, leaving it set to the start of the next sprite.
    ; `pilldraw` uses this to draw the second half of the two-part pill.
    mov bp,sp
    mov [bp+2],si
ds_ret:
    popa
    ret

; potentially place a virus at di, if there aren't too many already and
; the virus wins the dice roll
; -intended to be used with each_cell
%if enable_virii
place_virii:
    call rng
    cmp ah,VIRUS_THRESH
    jb pm_done ; return
    dec byte [bp+num_virii]
    js pm_done ; return if num_virii is underflowing
    mov si,sprite_virus ; draw virus sprite
    jmp draw_sprite
%endif

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
    jz c4_done ; ignore black cells
    mov cx,3
c4_check:
    add di,bx ; add offset
    cmp al,[di+COMMON_PIXEL] ; compare 3 offsets by bx
    jne c4_done ; if not equal, return
    loop c4_check
    mov cx,4 ; clear all 4 sprites
c4_clear:
    mov si,sprite_clear
    call draw_sprite ; al is set from above
    sub di,bx ; subtract offset
    loop c4_clear
    mov byte [bp+wait_flag],1 ; set wait flag
c4_done:
    pop di
c4_ret:
    ret

; remove any cleared pill piece in the cell
; di: top left corner
; intended to be used with each_cell
remove_cleared:
    test byte [di+CLEAR_PIXEL],0xFF
    jnz c4_ret ; near jump to return statement (2b)
    xor al,al
    jmp draw_sprite

; make pieces fall that can fall
%if enable_fallstuff
fall_stuff:
    cmp byte [di+VIRUS_PIXEL],0 ; virii don't have pixel set here
    je c4_ret ; if it is 0, then its a virus, don't make it fall
    cmp byte [di+8*320+COMMON_PIXEL],0 ; is there seomthing below?
    jnz c4_ret ; if it is not zero, there is something there
    mov byte [bp+wait_flag],1 ; set wait flag
    mov si,di ; set source
    add di,8*320 ; set target to be row below
    mov bx,8
fs_outer:
    mov cx,8 
fs_inner:
    movsb ; [di++] = [si++]
    mov byte [si-1],0
    loop fs_inner
    add di,312 ; increment di to next row of pixels
    add si,320 ; increment si to next row of pixels
    dec bx
    jnz fs_outer
    ret
%endif

; call a function in bx with di set to start of each cell on the board
each_cell:
    ; start at bottom right cell and work back.
    ; this is for place_virii so it can place most of them at the bottom.
    mov di,BOARD_END-CELL_SIZE*320-CELL_SIZE
ec_outer:
    mov cx,NUM_COLS
ec_inner:
    pusha
    call bx
    popa
    sub di,CELL_SIZE
    loop ec_inner
    ; if cx is 0, then subtract row
    sub di,CELL_SIZE*320-BOARD_WIDTH
    cmp di,BOARD_START
    ja ec_outer
    ret

; put a random color from colors array into al
; put a random byte into ah
rng:
%if enable_rng
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
%else
    mov ah,0xFF
    mov al,PILL_BLUE
    ret
%endif

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

times 510 - ($ - $$) db 0
dw 0xaa55
