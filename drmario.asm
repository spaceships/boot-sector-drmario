    bits 16

; conditional compilation of features
%assign enable_virii 1
%assign enable_rng 1
%assign enable_clear 1
%assign enable_singletons 0

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
CELL_SIZE:    equ 8             ; size of square board cells in pixels
SPRITE_ROWS:  equ 7             ; vertical size of sprite in pixels
SPRITE_COLS:  equ 7             ; horizontal size of sprite in pixels
BOARD_HEIGHT: equ CELL_SIZE*NUM_ROWS
BOARD_WIDTH:  equ CELL_SIZE*NUM_COLS
BOARD_START:  equ (100-BOARD_HEIGHT/2)*320+(160-BOARD_WIDTH/2)
BOARD_END:    equ (100+BOARD_HEIGHT/2)*320+(160+BOARD_WIDTH/2)
COMMON_PIXEL: equ 5
CLEAR_PIXEL:  equ 643 ; 2 pixels down, 3 right

;;;;;;;;;;;;;;;;;;;;
;; game variables ;;
;;;;;;;;;;;;;;;;;;;;
base:           equ 0xfc80
pill_loc:       equ 0                   ; [word]
pill_color:     equ pill_loc + 2        ; [word]
pill_offset:    equ pill_color + 2      ; [word]
pill_sprite:    equ pill_offset + 2     ; [word]
next_tick:      equ pill_sprite + 2     ; [word]
rand:           equ next_tick + 2       ; [word]
num_virii:      equ rand + 2            ; [byte]
clear_flag:     equ num_virii + 1       ; [byte]

start:
    mov bp,base   ; set base address for global state
    mov ax,0x0013 ; set video mode vga 320x200x256
    int 0x10      ; call bios interrupt

    int 0x1a ; bios clock read, reusing above's ah=0
    mov [bp+rand],dx ; initialize rand

    mov ax,0xa000 ; set data & extra segments to video
    mov ds,ax
    mov es,ax

    ;;;;;;;;;;;;;;;;;
    ;; draw border ;;
    ;;;;;;;;;;;;;;;;;
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

    ;;;;;;;;;;;;;;;;;
    ;; place virii ;;
    ;;;;;;;;;;;;;;;;;
    ; initialize num_virii 
    ; this is needed when you have multiple runs, like at game over. 
    ; otherwise, no virii appear.
%if enable_virii
    mov byte [bp+num_virii],al ; reuse al=0 from above
    mov ax,place_virii
    call each_cell
%endif

    ;;;;;;;;;;;;;;;;;;;;;
    ;; make a new pill ;;
    ;;;;;;;;;;;;;;;;;;;;;
pillnew:
    call rng
    mov cl,al
    call rng
    mov ah,cl
    mov [bp+pill_color],ax ; set colors
    mov di,BOARD_START+BOARD_WIDTH/2-CELL_SIZE ; initial pill loc
    mov bx,8 ; initial rotation
    mov word [bp+pill_sprite],sprite_left
    call pillmove_no_clear ; don't clear previous pill
    jnz start ; if occupied, restart game

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; initialization done, game loop follows ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
game_loop:
    mov di,[bp+pill_loc] ; pre-load pill location into di
    ;;;;;;;;;;;;;;;;;;;;
    ;; get user input ;;
    ;;;;;;;;;;;;;;;;;;;;
    ; get the actual key, if it is pressed
    mov ah,0x01 ; bios key available
    int 0x16 ; sets ZF=1 if no keystroke available
    je gl_check_left
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
    mov bx,8^(-8*320)
    xor bx,[bp+pill_offset] ; toggle between +8 (horiz) and -8*320 (vert)
    mov dx,((sprite_bottom-start)^(sprite_left-start))
    xor dx,[bp+pill_sprite] ; toggle between sprite_left and sprite_bottom
    test byte [di+COMMON_PIXEL+bx],0xFF
    jnz gl_clock ; no rotate
    call pillclear
    mov [bp+pill_offset],bx ; actually change offset
    mov [bp+pill_sprite],dx ; actually change sprite
    xor cl,bl               ; set 8 or 0 depending on orientation
    ror word [bp+pill_color],cl ; possibly swap colors
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
    ;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; remove cleared pills ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;
    push di
    mov ax,remove_cleared
    call each_cell
    pop di
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; make the pill fall ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;
pillfall:
    add di,8*320 ; move down 1 row
    call pillmove ; pillmove sets ZF when it is successful
    jz game_loop ; no obstructions, continue game loop
    ;;;;;;;;;;;;;;;;;;;;;;
    ;; check for clears ;;
    ;;;;;;;;;;;;;;;;;;;;;;
    mov ax,matchcheck ; check for a match, change it to sprite_clear
    call each_cell ; run matchcheck on every cell
%if enable_singletons
    mov ax,singletons ; convert half pills to singletons
    call each_cell ; run it on every cell
%endif
%if enable_clear
    test byte [bp+clear_flag],0xFF ; was the clear_flag set?
    jz pillnew ; if not, continue game
    call pause ; otherwise, pause
    mov ax,clearmarked ; clear cells that look like sprite_clear
    call each_cell ; run clearmarked on every cell
    mov byte [bp+clear_flag],0 ; reset clear_flag
%endif
    jmp pillnew ; continue game
    ;;;;;;;;;;;;;;;;;;;;;;
    ;; end of game loop ;;
    ;;;;;;;;;;;;;;;;;;;;;;

pause:
    mov cx,PAUSE_LEN
    mov ah,0x86
    int 0x15 ; bios wait function - cx:dx = interval in us
    ret

; di: proposed location
; sets ZF=1 if nothing is in the way
; sets pill_loc=di and pill_offset=bx if possible
pillmove:
    mov bx,[bp+pill_offset] ; load cur offset
    push di
    call pillclear ; clear pill avoiding interference
    pop di
pillmove_no_clear:
    mov al,[di+COMMON_PIXEL]
    or al,[di+COMMON_PIXEL+bx] ; is either cell occupied?
    jnz pm_restore ; if yes, restore pill and return
    mov [bp+pill_loc],di ; ok we're clear, move pill
    mov [bp+pill_offset],bx ; write new offset
pm_restore:
    pushf
    call pilldraw ; (re)draw pill
    popf
pm_done:
    ret

; di: top left pixel
; clobbers ax,si,di
pillclear:
    xor ax,ax
    jmp pd_common

; clobbers ax,si,di
pilldraw:
    mov ax,[bp+pill_color]
pd_common:
    mov di,[bp+pill_loc]
    mov si,[bp+pill_sprite]
    call draw_sprite
    ; `draw_sprite` leaves `si` set to the start of the next sprite
    mov al,ah
    add di,[bp+pill_offset]
    ; fall through to draw_sprite

; di: top left pixel
; si: start of sprite
; al: color
draw_sprite:
    pusha
    mov ah,al
    mov dx,SPRITE_ROWS
ds_row:
    cs lodsb  ; load byte of the sprite into al, advance si
    mov bl,al ; save it in bl
ds_row_again:
    mov cx,SPRITE_COLS  ; ds_col row runs 7 times
ds_col:
    xor al,al
    rol bl,1
    jnc ds_print ; if we just shifted off a 0, print black pixel
    mov al,ah    ; otherwise get the color
ds_print:
    stosb        ; print color to current pixel loc
    loop ds_col
    add di,320-7   ; increment di: +1 row, -8 cols
    ; The `ds_col` loop has run 7 times, rotating the original `bl` from
    ; `0xXXXXXXXR` 7 places to produce `0xRXXXXXXX`.  Shifting by 1 more bit
    ; copies R into the carry flag and sets `bl` to `0xXXXXXXX0`, so we'll draw
    ; the same row (if R is set) but won't repeat again afterward.
    shl bl,1
    dec dx       ; decrement row counter, preserving the carry flag
    jc ds_row_again
    jnz ds_row
ds_end:
    ; Preserve the final `si`, leaving it set to the start of the next sprite.
    ; `pilldraw` uses this to draw the second half of the two-part pill.
    mov bp,sp
    mov [bp+2],si
    popa
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

; potentially place a virus at di, if there aren't too many already and
; the virus wins the dice roll
; -intended to be used with each_cell
%if enable_virii
place_virii:
    cmp byte [bp+num_virii],VIRUS_MAX
    jae pm_done ; return
    call rng
    cmp ah,VIRUS_THRESH
    jb pm_done ; return
    inc byte [bp+num_virii]
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
    inc byte [bp+clear_flag] ; set global clear flag
c4_done:
    pop di
    ret

; remove any cleared pill piece in the cell
; di: top left corner
; intended to be used with each_cell
remove_cleared:
    test byte [di+CLEAR_PIXEL],0xFF
    jnz rc_done
    mov al,0
    mov si,sprite_clear
    call draw_sprite
rc_done:
    ret

; call a function in ax with di set to start of each cell on the board
each_cell:
    ; start at bottom right cell and work back.
    ; this is for place_virii so it can place most of them at the bottom.
    mov di,BOARD_END-CELL_SIZE*320-CELL_SIZE
ec_outer:
    mov cx,NUM_COLS
ec_inner:
    pusha
    call ax
    popa
    sub di,CELL_SIZE
    loop ec_inner
    ; if cx is 0, then subtract row
    sub di,CELL_SIZE*320-BOARD_WIDTH
    cmp di,BOARD_START
    ja ec_outer
    ret

; clear all cells which are marked for deletion
%if enable_clear
clearmarked:
    test byte [di+2*320+3],0xFF ; unique 0 pixel in sprite clear
    jnz pm_done ; if occupied, return
    xor al,al  ; otherwise, draw empty cell
    jmp draw_sprite
%endif

; convert half pills into singletons, assuming di is a clear
%if enable_singletons
singletons:
    mov bx,8 ; test right
    call single_test
    neg bx ; test left
    call single_test
    mov bx,8*320 ; test below
    ; call single_test
    ; neg bx ; test above
    ; fallthrough to single_test

single_test:
    mov al,[di+bx]
    or al,[di+bx+6]
    or al,[di+bx+7*320]
    or al,[di+bx+7*320+6]
    jz pm_done ; single or clear
    ; TODO: test for virus and border
    cmp al,BORDER_COLOR
    je pm_done
    pusha
    mov si,sprite_single
    call draw_sprite
    popa
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
