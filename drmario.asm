    bits 16

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; adjustable parameters ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
PILL_RED:       equ 0x27
PILL_YELLOW:    equ 0x2c
PILL_BLUE:      equ 0x36
BORDER_COLOR:   equ 0x6b
SPEED:          equ 4
VIRUS_COUNT:    equ 8
NUM_ROWS:       equ 16  ; Original: 16
NUM_COLS:       equ 8   ; Original: 8

;;;;;;;;;;;;;;;;;;;;;;
;; fixed parameters ;;
;;;;;;;;;;;;;;;;;;;;;;
SPRITE_SIZE:  equ 8
BOARD_HEIGHT: equ SPRITE_SIZE*NUM_ROWS
BOARD_WIDTH:  equ SPRITE_SIZE*NUM_COLS
BOARD_START:  equ (100-BOARD_HEIGHT/2)*320+(160-BOARD_WIDTH/2)
BOARD_END:    equ (100+BOARD_HEIGHT/2)*320+(160+BOARD_WIDTH/2)
COMMON_PIXEL: equ 5

;;;;;;;;;;;;;;;;;;;;
;; game variables ;;
;;;;;;;;;;;;;;;;;;;;
base:           equ 0xfc80
pill_loc:       equ base                ; [word]
pill_color:     equ pill_loc + 2        ; [word]
pill_offset:    equ pill_color + 2      ; [word]
pill_sprite:    equ pill_offset + 2     ; [word]
next_tick:      equ pill_sprite + 2     ; [word]
rand:           equ next_tick + 2       ; [word]

start:
    mov ax,0x0013   ; set video mode vga 320x200x256
    int 0x10        ; call bios interrupt

    mov ax,0xa000   ; set data & extra segments to video
    mov ds,ax
    mov es,ax

    mov ah,0x00
    int 0x1a      ; bios clock read
    mov [rand],dx ; initialize rand

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
    mov bx,BOARD_HEIGHT
    mov di,BOARD_START
db_row:
    mov cx,BOARD_WIDTH
    rep stosb
    add di,320-BOARD_WIDTH
    dec bx
    jnz db_row

    ;;;;;;;;;;;;;;;;;
    ;; place virii ;;
    ;;;;;;;;;;;;;;;;;
    mov dx,VIRUS_COUNT
    ; start at bottom right sprite and work back
    mov di,BOARD_END-SPRITE_SIZE*320-SPRITE_SIZE 
    mov si,sprites+7*SPRITE_SIZE ; draw virus sprite
pv_row:
    mov cx,NUM_COLS
pv_loop:
    call rng
    cmp al,210
    jb pv_continue
    call rand_color
    call draw_sprite
    dec dx ; decrement virus count
    jz pv_done
pv_continue:
    sub di,SPRITE_SIZE
    loop pv_loop
    ; if cx is 0, then subtract row
    sub di,SPRITE_SIZE*320-BOARD_WIDTH
    cmp di,BOARD_START
    ja pv_row
pv_done:
    
    ;;;;;;;;;;;;;;;;;;;;;
    ;; make a new pill ;;
    ;;;;;;;;;;;;;;;;;;;;;
    call pillnew

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; initialization done, game loop follows ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
game_loop:
    ;;;;;;;;;;;;;;;;;;;;
    ;; get user input ;;
    ;;;;;;;;;;;;;;;;;;;;
    ; preamble, setting up movement
    mov bx,8        
    mov cx,[pill_offset]
    test cx,cx ; is pill offset negative? pill veritcal?
    js gl_offset_ok
    shl bx,1        ; R horizontal, mul bx by 2
    xor cx,cx       ; LR/horiz: only check 1 place
gl_offset_ok:

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
    jne gl_check_s
    call pillrot

gl_check_s:
    cmp ah,0x1f ; 's'
    jne gl_clock
    ; swap colors
    mov ax,[pill_color]
    xchg al,ah
    mov [pill_color],ax
    call pilldraw

    ;;;;;;;;;;;;;;;;
    ;; game clock ;;
    ;;;;;;;;;;;;;;;;
gl_clock:
    mov ah,0x00
    int 0x1a    ; bios clock read
    cmp dx,[next_tick]
    jb game_loop
    add dx,SPEED
    mov [next_tick],dx
    ;;;;;;;;;;;;;;;;;;;;;;;;
    ;; make the pill fall ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;
    call pillfall
    jmp game_loop
    ;;;;;;;;;;;;;
    ;; the end ;;
    ;;;;;;;;;;;;;

pillnew:
    mov word [pill_loc],BOARD_START+BOARD_WIDTH/2-SPRITE_SIZE
    mov word [pill_offset],8
    mov word [pill_sprite],sprites+3*8
    call rand_color
    mov cl,al
    call rand_color
    mov ah,cl
    mov [pill_color],ax
    call pilldraw
    ret

pillrot:
    mov bx,-8*320              ; new offset is vert
    mov cx,sprites+SPRITE_SIZE ; new sprites are vert
    cmp word [pill_offset],0   ; are we currently horiz?
    jg gl_rotate_test
    mov bx,8                   ; set offset to horiz
    add cx,2*SPRITE_SIZE       ; set sprites to horiz
gl_rotate_test:
    mov di,[pill_loc] 
    test byte [di+COMMON_PIXEL+bx],0xFF
    jnz pm_done                ; no rotate, return
gl_rotate_ok:
    call pillclear
    mov [pill_offset],bx ; actually change offset
    mov [pill_sprite],cx ; actually change sprite
    jmp pilldraw

pillfall:
    mov ax,8*320
    mov bx,ax
    mov cx,[pill_offset] ; cx will be 8 or -8*320
    test cx,cx  ; if cx is negative, cancel it out
    jns pf_call ; because we only have to test 1 loc
    xor cx,cx   ; when falling with vertical pill
pf_call:
    call pillmove ; pillmove sets ZF when it is successful
    jz pm_done    ; reuse pillmove's ret statement 
    ; there is something in the way, check for clears

;     mov cx,4
;     mov di,[pill_loc]
; pf_checkrow:
;     call clearrow
;     sub di,SPRITE_SIZE
;     loop pf_checkrow

pf_done: 
    jmp pillnew

; clearrow:
;     mov dl,[di+COMMON_PIXEL]
;     mov al,dl 
;     and al,[di+SPRITE_SIZE+COMMON_PIXEL]
;     and al,[di+2*SPRITE_SIZE+COMMON_PIXEL]
;     and al,[di+3*SPRITE_SIZE+COMMON_PIXEL]
;     cmp dl,al
;     jne pm_done
;     ; clear!
;     mov si,sprites ; zero sprite
;     mov cx,4
; cr_clear:
;     call draw_sprite
;     add di,SPRITE_SIZE
;     loop cr_clear
;     ret

; clearcol:
;     ret 

; ax: moving offset
; bx: checking offset 1
; cx: checking offset 2 (in addition to bx)
pillmove:
    mov di,[pill_loc]
pm_test:
    test byte [di+COMMON_PIXEL+bx],0xFF
    jnz pm_done
    add bx,cx
    test byte [di+COMMON_PIXEL+bx],0xFF
    jnz pm_done
pm_move:
    call pillclear
    add word [pill_loc],ax
    call pilldraw
    xor ax,ax ; resets ZF=1 for pillfall
pm_done:
    ret

pillclear:
    mov di,[pill_loc]
    mov si,sprites
    call draw_sprite
    jmp pd_draw_sprite ; reuse the bottom 2 lines of pilldraw

pilldraw:
    mov di,[pill_loc]
    mov ax,[pill_color]
    mov si,[pill_sprite]
    call draw_sprite
    mov al,ah
    add si,SPRITE_SIZE
pd_draw_sprite:
    add di,[pill_offset]
    jmp draw_sprite

; di: top left pixel
; si: start of sprite
; al: color
draw_sprite:
    pusha
    mov ah,al
    mov cx,8
ds_row:
    push cx
    mov cx,8
    cs lodsb  ; load byte of the sprite into al, advance si
    mov bl,al ; save it in bl
ds_col:
    xor al,al
    shl bl,1
    jnc ds_print ; if we just shifted off a 0, print black pixel
    mov al,ah    ; otherwise get the color
ds_print:
    stosb        ; print color to current pixel loc
    loop ds_col
    add di,312   ; increment di
    pop cx
    loop ds_row  ; if row != 8, jump to ds_row
    popa
    ret

; ; lfsr, sets ax, clobbers bx
; rng:
;     mov ax,[rand]
;     mov bx,ax
;     and bx,0b1000000000101101
;     shr ax,1
;     xor bl,bh
;     jpe rng_done
;     or ax,0x8000
; rng_done:
;     mov [rand],ax
;     ret

rng:
    mov ax,[rand]
    mov bx,ax
    xor bl,bh
    rcr bl,2      ; put bit 1 into carry flag
    rcr ax,1
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
    ret

sprites:
    ; 0: clear
    dw 0x00,0x00,0x00,0x00
    ; 1: pill bottom
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b11111110
    db 0b01111100
    db 0b00000000
    ; 2: pill top
    db 0b01111100 
    db 0b11011110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b10111110
    db 0b11111110
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
    ; 7: virus
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
