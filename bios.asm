PORTA 	equ 0x4
PORTB 	equ 0x5
PORTAC 	equ 0x6
PORTBC 	equ 0x7
SERA 	equ 0x8
SERB 	equ 0x9
SERAC 	equ 0xa
SERBC 	equ 0xb
	
DSP_CLR equ 1
DSP_RH	equ 2
DSP_MI	equ 6
DSP_ON	equ 14
DSP_IW	equ 0b00000001
DSP_IR	equ 0b00000011
DSP_DW	equ	0b00000101
SPADDR	equ 0x4000


CLK_CH0 equ 0x10
CLK_CH1 equ 0x11
CLK_CH2 equ 0x12
CLK_CH3 equ 0x13


#target ROM
#code _BOOT
#code _GSINIT
#code _INTERRUPT
#code _BIOS_CODE
#code _END, *, 0x800 - _BIOS_CODE_end
#data _BIOS_RAM, 0x2000, 0x80
#data _WORK_RAM, *, 0x2048
#data _STACK_RAM, *, 0x80

#code _BOOT
	; init stack address
	ld hl, SPADDR
	ld sp, hl

	jp main
	halt

	org 0x10
	defw int_ctc_ch0
	defw int_ctc_ch1
	defw int_ctc_ch2
	defw int_ctc_ch3
	halt

#code _INTERRUPT
	org 0x038
	di

	call interrupt_handler

	ei
	reti

	; resume code
#code _BIOS_CODE
	org 0x43


SERIAL_INIT
	defb 0, 0b00011000	; WR0, channel reset
	defb 4, 0b11000100  ; WR4, no parity, 1 stop bit, x64 clock mode
	defb 3, 0b11000001	; WR3, Rx enable, Rx 8 bits /char
	defb 5, 0b01101000	; WR5, Tx enable, Tx 8 bitts char
	defb 1, 0b10000000	; WR1, Wait/Ready on R/T enable 

#data _BIOS_RAM
; sub the same char from the last char to make it byte \00
LCD1 defs 20
LCD2 defs 20
CARRET_POS defs 1
DISP_LINE defs 1
TMP_STR defs 20

#data _WORK_RAM
APP_LOC defs 0x2048

#code _BIOS_CODE

initpio:
	; set ouput mode on PIO 
	ld a, 0xf	; b00001111 is control byte for output
	; b1 addresses pio in command mode
	out PORTAC, a
	out PORTBC, a
	ret

initserial:
	push hl
	ld hl, SERIAL_INIT
	ld b, 10	; size of serial init code 5 registers(a byte address) x 1 byte config
	ld c, SERAC
	otir

	pop hl
	ret

wait:
	push bc
	ld b, 255
#local
wloop:
	nop
	djnz wloop
#endlocal
	pop bc
	ret

; wait for 255^2 machine cycles
exwait:
	push bc
	ld b, 255
#local
eloop:
	call wait
	djnz eloop
#endlocal
	pop bc
	ret

; wait for 255^2*5 machine cycles
longwait:
	push bc
	ld b, 5
#local
lloop:
	call exwait
	djnz lloop
#endlocal
	pop bc
	ret


pioout_slow:
	; output to pio
	ld a, d
	out PORTB, a
	ld a, e
	out PORTA, a
	ld a, 0
	out PORTB, a
	call wait
	ret

sioout:
	ld a, e
	out SERA, a
	ret


charout:
	push af
	push bc
	push hl
	push de

	; serial out
	ld d, DSP_DW
	call sioout

	; check if new line, then do line feed instead
	ld a, e
	sub 13
	jr z, skip

	ld a, e
	sub 8
	jr z, backspace

	ld a, e
	sub 10
	jr z, newline_char

	; display out
	call pioout

	ld hl, CARRET_POS
	ld b, 0
	ld c, (hl)
	ld hl, LCD2
	add hl, bc
	ld (hl), e
	
	ld hl, CARRET_POS
	inc (hl)

skip:
backspace:
	pop de
	pop hl
	pop bc
	pop af
	ret

newline_char:
	call line_feed
	jr skip


;; output string

mstringout:
	ld d, DSP_DW
soutloop:
	ld a, (hl)
	or a
	ret z

	ld e, (hl)
	call charout
	inc hl
	jp soutloop

raw_textout:
#local
	ld d, DSP_DW
nextchar:
	ld a, (hl)
	or a
	ret z

	ld e, (hl)
	call pioout
	inc hl
	jp nextchar
#endlocal

default_lcd_lines:
	push hl
	ld bc, 20
	ld de, LCD1
	ld hl, WELCOME
	ldir
	ld bc, 20
	ld de, LCD2
	ld hl, SHELL
	ldir
	pop hl
	ret

cleardisplay:
	; clear display
	ld d, DSP_IW
	ld e, 0b00000001
	call pioout
	call wait
	call wait
	ret

set_carret_home:
	push hl
	ld d, DSP_IW
	ld e, 0b10000000
	call pioout
	ld hl, CARRET_POS
	ld (hl), 0	
	pop hl
	ret	

set_carret_2nd_line:
	ld d, DSP_IW
	ld e, 0b11000000
	call pioout
	ret	

set_carret_raw:
	; b - offset, 64 (0x40) is 2nd line
	; c - offset on the line
	push af

	; update memory carret position
	push hl
	ld hl, CARRET_POS
	ld (hl), bc
	pop hl

	ld d, DSP_IW
	ld a, 0x80
	add b
	add c
	ld e, a
	call pioout
	pop af
	ret

return_shell:
	push hl
	; clear shell line
	ld bc, 20
	ld de, LCD2
	ld hl, SHELL
	ldir

	call set_carret_2nd_line
	ld hl, LCD2
	call raw_textout

	call set_carret_2nd_line
	ld hl, DISP_LINE
	ld a, (hl)
	add 48
	ld d, DSP_DW
	ld e, a
	call pioout

	; position carret
	ld d, DSP_IW
	ld e, 0b11000010
	call pioout

	ld hl, CARRET_POS
	ld (hl), 2

	pop hl
	ret

line_feed:
	push hl
	; copy lcd2 into lcd1
	ld bc, 20
	ld hl, LCD2
	ld de, LCD1
	ldir

	ld a, (DISP_LINE)
	add 48
	ld hl, LCD1
	ld (hl), a

	ld hl, DISP_LINE
	inc (hl)

	call set_carret_home

	ld hl, LCD1
	call raw_textout

	call return_shell
	pop hl
	ret

initdisplay:
	; function set
	ld d, DSP_IW
	ld e, 0b00111000
	call pioout
	call wait
	call wait

	; display on 
	ld d, DSP_IW
	ld e, 0b00001111
	call pioout
	call wait
	call wait

	; increment mode
	ld d, DSP_IW
	ld e, 0b00000110
	call pioout
	call wait
	call wait

	call cleardisplay
	ret

;IN    HL     Address of string1.
;      DE     Address of string2.
;OUT   zero   Set if string1 = string2, reset if string1 != string2.
;      carry  Set if string1 > string2, reset if string1 <= string2.

CmpStrings:
    PUSH   HL
    PUSH   DE

    LD     A, (DE)          ; Compare lengths to determine smaller string
    CP     (HL)            ; (want to minimize work).
    JR     C, Str1IsBigger
    LD     A, (HL)

Str1IsBigger:
    LD     C, A             ; Put length in BC
    LD     B, 0
    INC    DE              ; Increment pointers to meat of string.
    INC    HL

CmpLoop:
    LD     A, (DE)          ; Compare bytes.
    CPI
    JR     NZ, NoMatch      ; If (HL) != (DE), abort.
    INC    DE              ; Update pointer.
    JP     PE, CmpLoop

    POP    DE
    POP    HL
    LD     A, (DE)          ; Check string lengths to see if really equal.
    CP     (HL)
    RET

NoMatch:
    DEC    HL
    CP     (HL)            ; Compare again to affect carry.
    POP    DE
    POP    HL
    RET

reset:
	call default_lcd_lines
	ld hl, DISP_LINE
	ld (hl), 0
	; this is the first init
	call set_carret_home
	ld hl, LCD1
	call raw_textout
	call return_shell
	ret


LOAD_TITLE1 .asciz "Loading programm:"
load:
	call set_carret_home
	ld hl, LOAD_TITLE1
	call raw_textout

	call set_carret_2nd_line
	ld a, e
	add 48
;	ld e, a
;	call charout
	
	; load destination in RAM
	ld hl, APP_LOC
#local
	; set load buffer size to 0 / init state
	ld d, 0
	ld e, 0
	ld (hl), de

loadpage:
	; wait for character to appear
	in a, (SERAC)
	bit 0, a
	jr z, loadpage

	; read page size
	in a, (SERA)
	or a
	jp z, allpagesloaded ; if the page size is 0, jump to finish

; ====================================

	; store current page size in register pari BC
	ld b, 0
	ld c, a

	ld hl, APP_LOC
	ld de, (hl)
	; first 2 bytes are the size of the app, skip those
	inc hl
	inc hl
	; move offset
	add hl, de

; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

	; set number of bytes to read to current page size (stored in C register)
	push bc
	ld b, c
	ld c, SERA ; we want to read from port SERA
nextserial:
	; wait for character ready
	in a, (SERAC)
	bit 0, a
	jr z, nextserial

	; read next byte into (hl)
	ini
	jr nz, nextserial
	pop bc

	; do 16 bit addition of the loaded memory, kind of hacky to us HL register for that
	ld hl, APP_LOC
	ld de, (hl)
	; current app size loaded
	ld hl, de
	; add the current page size
	add hl, bc
	; move it to de proxy register as we can only reference memory via (hl)
	ld de, hl
	; load the app location & push the new calculated app size
	ld hl, APP_LOC
	ld (hl), de

	; go to next page load
	jp loadpage

	;jp loadpage
allpagesloaded:

	; return carret
	ld b, 64
	ld hl, CARRET_POS
	ld c, (hl)
	call set_carret_raw

	call longwait

	call reset
#endlocal
	ret

display_app_bytes:
	push bc
	push de
	ld hl, APP_LOC
	ld b, (hl)
	inc hl
outbytes:
	ld e, (hl)
	inc hl
	call charout
	djnz outbytes

	call longwait
	pop de
	pop bc
	ret


peek0:
#local
	push af
	ld b, 0
	ld c, 10
	call set_carret_raw
	ld hl, APP_LOC
	pop af

	ld c, a
	ld b, 0
	add hl, bc

	ld d, DSP_DW
	ld a, (hl)
	add 48
	ld e, a
	call pioout
	call sioout

	ld b, 64
	ld hl, CARRET_POS
	ld c, (hl)
	call set_carret_raw

#endlocal
	ret	

; 0..9 = 48..57
; a..f = 97..102

NumToHex:
	ld c, a   ; a = number to convert
	call Num1
	ld d, a
	ld a, c
	call Num2
	ld e, a
	ret  ; return with hex number in de

Num1:
	rra
    rra
    rra
    rra
Num2:
	or $F0
    daa
    add a, $A0
    adc a, $40 ; Ascii hex at this point (0 to F)   
    ret

peek:
#local
	push af
	ld b, 0
	ld c, 10
	call set_carret_raw
	pop af

	; evaluationg peek
	push af
	push bc
	call NumToHex
	ld b, a 

	ld d, DSP_DW

	ld e, '='
	call sioout
	ld e, '>'
	call sioout
	ld e, ' '
	call sioout
	ld e, 'p'
	call sioout
	ld e, 'e'
	call sioout
	ld e, 'e'
	call sioout
	ld e, 'k'
	call sioout
	ld e, '['
	call sioout
	ld e, b
	call sioout
	ld e, ']'
	call sioout
	ld e, ':'
	call sioout
	ld e, ' '
	call sioout
	pop bc
	pop af

	ld hl, APP_LOC
	; calculate address for peek byte
	ld c, a
	ld b, 0
	add hl, bc

	ld a, (hl)
	call NumToHex

	; store lsb in c register
	ld c, e
	ld b, d

	ld d, DSP_DW
		
	; ld e, (hl)
	; b register contains lsb
	ld e, b
	call pioout
	call sioout

	; c register contains msb
	ld e, c
	call pioout
	call sioout

	ld b, 64
	ld hl, CARRET_POS
	ld c, (hl)
	call set_carret_raw

	ld e, 10
	call sioout

#endlocal
	ret	

; this will peek and display memory location in ram starting at APP_LOC (0xe00)
peek_pos:
#local	
	; we need to move past [len] + 'peek ' string - 6 bytes
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl
	inc hl

	; decimal input needs to be converted to byte, eg 10 [49h48h] => 0xa
	ld d, (hl)
	inc hl
	ld e, (hl)
;	ld d, '0'
;	ld e, '2
	call HexToNum ; we have decoded value in A register

	; move carret to 1st row 10th column for result display pos
	ld b, 0
	ld c, 10
	call set_carret_raw

	; we have address offset in a
	ld hl, APP_LOC
	or a
	jp z, no_offset
	ld b, a
next_offset:	
	inc hl
	djnz next_offset
no_offset:

;	add hl, bc

	; encode byte value to hex for display
	ld a, (hl)
	call NumToHex
	ld c, e
	ld b, d

	; output value
	ld d, DSP_DW		
	; b register contains lsb
	ld e, b
	call pioout
	; c register contains msb
	ld e, c
	call pioout

	; reset carret back to the next row
	ld b, 64
	ld hl, CARRET_POS
	ld c, (hl)
	call set_carret_raw

#endlocal
	ret

HexToNum:
   ld   a,d
   call Hex1
   add  a,a
   add  a,a
   add  a,a
   add  a,a
   ld   d,a
   ld   a,e
   call Hex1
   and 	0xf
   or   d
   ret

Hex1:
   sub  '0'
   cp   10
   ret  c
   sub  'A'
   add  a, '0'
   add a, 10
   ret

RUN_APP_TXT .ascii "Running app...!", 0x0
run_app:
	call cleardisplay

	ld hl, RUN_APP_TXT
	call mstringout
	call longwait

	ld hl, APP_LOC
	; first 2 bytes are app size
	inc hl
	inc hl
	jp (hl)
app_ret:
	call reset
	ret


copy_screen_to_temp:
	; hl should be
	ld bc, 20
	ld hl, LCD1
	inc hl
	inc hl
	ld de, TMP_STR
	ld a, c
	ld (de), a
	inc de
	ldir
	inc de
	ld a, 0
	ld (de), a
	ret

RESET_CMD .ascii 5, "reset", 0x0
LOAD_CMD .ascii 4, "load", 0x0
RUN_CMD .ascii 3, "run", 0x0
PEEK_CMD .ascii 5, "peek ", 0x0
MONITOR_CMD .ascii 7, "monitor", 0x0
commandeval:

#local
	push af
	ld a, e
	sub 10
	jr z, eval
	pop af
	ret

eval:
	pop af
#endlocal

	ld bc, 10
	call copy_screen_to_temp

	; comparison
	ld hl, TMP_STR
	ld (hl), 5
	ld de, RESET_CMD
	call CmpStrings
	call z, reset

	; check load
	ld hl, TMP_STR
	ld (hl), 4
	ld de, LOAD_CMD
	call CmpStrings
	call z, load

	; run command
	ld hl, TMP_STR
	ld (hl), 3
	ld de, RUN_CMD
	call CmpStrings
	call z, run_app

	; peek ram for app addresses


	ld hl, TMP_STR
	ld (hl), 5
	ld de, PEEK_CMD
	call CmpStrings
	call z, peek_pos

	ld hl, TMP_STR
	ld (hl), 7
	ld de, MONITOR_CMD
	call CmpStrings
	call z, monitor

	ret

waitserial:
#local
poll:
	in a, (SERAC)
	bit 0, a
	jr z, poll
#endlocal
	in a, (SERA)
	ld e, a
	ret

; serial & keyboard input

; move to BIOS
OutHex:
	push bc
	push de
	push af
	call NumToHex
	ld bc, de
	ld d, DSP_DW

#local
	ld a, b
	sub a, 48
	jp z, singlechar

	ld e, b
	call pioout

singlechar:
	ld e, c
	call pioout
#endlocal

	pop af
	pop de
	pop bc
	ret


; TODO: move to BIOS
disablecursor:
	; hide cursor
	ld d, DSP_IW
	ld e, 0b00001100
	call pioout
	ret

; move to BIOS
#data _BIOS_RAM
keystroke defs 4, 00h
#code _BIOS_CODE
wait_keystroke:
	push hl
	push bc
	push de
	ld hl, keystroke
#local	
	ld (hl), 3
	inc hl
	ld (hl), 0
	inc hl
	ld (hl), 0
	inc hl
	ld (hl), 0
	ld hl, keystroke

; we wait for at least one keycode
poll:
	in a, (SERAC)
	bit 0, a
	jr z, poll

	ld b, 0
	inc b
	; we have a valid first code so increment hl[0] to 1
	; load the code into hl[1]
	inc hl
	in a, (SERA)
	ld (hl), a

	;jp done
	; is there a 2nd keycode right away?
	in a, (SERAC)
	bit 0, a
	jp z, done

	inc b
	inc hl

	in a, (SERA)
	ld (hl), a

	; is there a 3rd keycode right away?
	in a, (SERAC)
	bit 0, a
	jp z, done

	inc b
	inc hl

	in a, (SERA)
	ld (hl), a

done:
#endlocal
	ld hl, keystroke
	ld (hl), b
	pop de
	pop bc
	pop hl
	ret


PS2_SERIAL_INIT
	defb 0, 0b00011000	; WR0, channel reset
	defb 4, 0b00000101  ; WR4, odd parity, 1 stop bit, x1 clock mode coming from keyboard
	defb 3, 0b11000001	; WR3, Rx enable, Rx 8 bits /char
	defb 5, 0b01101000	; WR5, Tx enable, Tx 8 bitts char
	defb 1, 0b10000000	; WR1, Wait/Ready on R/T enable 

init_ps2_keyboard:
	push hl
	push bc

	ld hl, PS2_SERIAL_INIT
	ld b, 10	; size of serial init code 5 registers(a byte address) x 1 byte config
	ld c, SERBC
	otir

	pop bc
	pop hl
	ret

; waits on a scancode via serial port B that is wired up to PS/2 keyboard
#data _BIOS_RAM
scancode defs 4, 00h
#code _BIOS_CODE
wait_ps2_scancode:
	push hl
	push bc
	push de
	ld hl, scancode
	ld (hl), 3
	inc hl
	ld (hl), 0
	inc hl
	ld (hl), 0
	inc hl
	ld (hl), 0
	ld hl, scancode

#local
	jp poll

sink_break_code:
	in a, (SERBC)
	bit 0, a
	jr z, sink_break_code

	in a, (SERB)
	pop af
	jp done

poll:
	in a, (SERBC)
	bit 0, a
	jr z, poll

	ld b, 0
	inc b
	; we have a valid first code so increment hl[0] to 1
	; load the code into hl[1]
	inc hl
	in a, (SERB)

	push af
	; ignore break codes
	sub 0xf0
	jp z, sink_break_code
	pop af

	; real scancode
	ld (hl), a

	;jp done
	; is there a 2nd scancode right away?
	in a, (SERBC)
	bit 0, a
	jp z, done

	inc b
	inc hl

	in a, (SERB)
	ld (hl), a

	; is there a 3rd scancode right away?
	in a, (SERBC)
	bit 0, a
	jp z, done

	inc b
	inc hl

	in a, (SERB)
	ld (hl), a

done:
	ld hl, scancode
	ld (hl), b
#endlocal
	pop de
	pop bc
	pop hl
	ret

pioout:
DSP_IRC	equ 0b00000010
	; ensure device is not busy
	; switch PIO to read mode

	ld a, 0
	out PORTA, a

	; load flag read
	ld a, DSP_IR
	out PORTB, a

	ld a, 0b01001111
	out PORTAC, a

#local
	push bc

	; max wait time
	ld b, 255

wait_busy:
	ld a, DSP_IRC
	out PORTB, a

	ld a, DSP_IR
	out PORTB, a

	in a, PORTA
	and a, 0b10000000

	jr z, busy_clear

	djnz wait_busy 

busy_clear:
	ld a, DSP_IRC
	out PORTB, a

#endlocal
	pop bc

	ld a, 0b00001111
	out PORTAC, a

	; output to pio & wait until busy flag clears
	ld a, d
	out PORTB, a
	ld a, e
	out PORTA, a
	ld a, 0
	out PORTB, a

	ret

WELCOME .asciz 	"gazz80, v0.34      "
SHELL .asciz 	" >                 "
INT_YO .asciz 	"Interrupt!!!"


interrupt_handler:
	
	call cleardisplay

	ld b, 0
	ld c, 0
	call set_carret_raw

	ld hl, INT_YO
	call raw_textout

	ret

main:
	call initpio
	call initdisplay
	call init_ps2_keyboard
	call initserial
	call init_clock
	call init_interrupts

	ld hl, keystroke
	ld (hl), 0
	ld hl, scancode
	ld (hl), 0

	call reset

runloop:
	call waitserial
	call charout

	call commandeval

	jp runloop

	halt


; output 8 bytes at any address to serial
set_def_mon_address:
	push hl
	ld hl, MONITOR_ADDR
	ld (hl), 0xff
	inc hl
	ld (hl), 0xff
	inc hl
	ld (hl), 0xff
	inc hl
	ld (hl), 0xff
	pop hl
	ret

monitor:
#data _BIOS_RAM
MONITOR_ADDR defs 4, 0x0123

#code _BIOS_CODE

#local
	push af
	ld b, 0
	ld c, 10
	call set_carret_raw
	pop af

	call set_def_mon_address

	ld hl, TMP_STR
	ld b, 8
skip_command:
	inc hl
	djnz skip_command

	; first char
	ld d, (hl)
	inc hl
	ld e, (hl)
	; store TMP_STR cursor
	inc hl
	push hl
	call HexToNum ; we have decoded value in A register
	ld hl, MONITOR_ADDR
	ld (hl), a

	; second char
	; pop TMP_STR cursor
	pop hl
	ld d, (hl)
	inc hl
	ld e, (hl)
	call HexToNum ; we have decoded value in A register

	ld hl, MONITOR_ADDR
	inc hl
	ld (hl), a

	ld d, DSP_DW
	ld e, '='
	call sioout
	ld e, '>'
	call sioout
	ld e, ' '
	call sioout
	ld e, '0'
	call sioout
	ld e, 'x'
	call sioout

	; output monitor address
	ld hl, MONITOR_ADDR
	ld a, (hl)
	call NumToHex

	ld b, e
	ld e, d
	ld d, DSP_DW
	call sioout
	ld e, b
	call sioout

	inc hl
	ld a, (hl)
	call NumToHex

	ld b, e
	ld e, d
	ld d, DSP_DW
	call sioout
	ld e, b
	call sioout

	ld e, ':'
	call sioout
	ld e, ' '
	call sioout


	ld hl, (MONITOR_ADDR)
	ld de, hl
	ld h, e
	ld l, d

	ld b, 16
	ld c, 0
next_byte:
	ld a, (hl)
	call NumToHex
	push bc
	ld c, e
	ld b, d

	; write out	
	ld d, DSP_DW
	ld e, b
	call pioout
	call sioout
	ld e, c
	call pioout
	call sioout

	pop bc

	inc hl

	ld a, (hl)
	call NumToHex
	push bc
	ld c, e
	ld b, d

	; write out	
	ld d, DSP_DW
	ld e, b
	call pioout
	call sioout
	ld e, c
	call pioout
	call sioout

	pop bc

	inc hl

	ld e, ' '
	call sioout

	djnz next_byte

	; newline
	ld e, 10
	call sioout

#endlocal
	ret	


DigitU8_ToASCII_3digit:
	push af
	push bc
	ld b, a
	ld a, 0
	sbc 1
	ld a, b
	call DigitU8_ToASCII
	pop bc
	pop af
	ret

; converts unsigned 8bit digit in A register to 1..3 ascii characters
; hl is expected to hold memory target area of 4 bytes for zero terminated string
DigitU8_ToASCII:
	push hl
	push bc
	push af
#local
	call convert
	inc hl
	ld (hl), 0
	pop af
	pop bc
	pop hl
	ret

convert:
	jp nc, twodigits 
	ld	c,-100
	call	Na1
	inc hl
twodigits:
	ld	c,-10
	call	Na1
	inc hl
	ld	c,-1
Na1:
	ld	b,'0'-1
Na2:
	inc	b
	add	a,c
	jr	c,Na2
	sub	c		;works as add 100/10/1
	push af		;safer than ld c,a
	ld (hl), b
	;ld	a,b		;char is in b
#endlocal
	pop af		;safer than ld a,c
	ret

; converts unsigned 16bit digit in DE register pair to 1..5 ascii characters
; hl is expected to hold memory target area of 6 bytes for zero terminated string
DigitU16_ToASCII:
	push hl
	push bc
	push af	
#local
	call convert16
	inc de
	ld a, 0
	ld (de), a
	pop af
	pop bc
	pop hl
	ret

convert16:
Num2Dec:
	ld	bc,-10000
	call	Num1
	; if result is 0 then go back one byte in destination string to trim it out

no_10k:
	ld	bc,-1000
	call	Num1
	
;	call trim_zero
;	jr z, no_1k
;	inc de

no_1k:
	ld	bc,-100
	call	Num1


no_100:
	ld	c,-10
	call	Num1
	ld	c,b

Num1	ld	a,'0'-1
Num2	inc	a
	add	hl,bc
	jr	c,Num2
	sbc	hl,bc

	ld	(de),a
	inc	de
	ret

trim_zero:
	dec de
	ld a, (de)
	cp a, '0'
	ret
#endlocal


;; *******************************************************************************
;; **																			**
;; **	Clock																	**
;; **																			**
;; *******************************************************************************

init_clock:

	; A channel control word and a time constant data word must be written to the appropriate registers of that channel

	; * In CTC COUNTER mode, the CTC counts edges of the CLK/TRG input	
	;	---

	; * In CTC TIMER mode, the CTC generates timing intervals that are an integer value of the system clock period
	;	--- phi * P * TC: phi = System clock, P = prescaler, TC = time constant

	; interrupt vector, since I points to the correct address, we just set it to 0
	push af
	push bc
	push hl

	ld a, 0x10
	out CLK_CH0, a

	; lets try counter mode
	; CH 0
	ld a, 0b01000111
	out CLK_CH0, a
	ld a, 0b00000001
;	ld a, 0b00000000
	out CLK_CH0, a

	; TC:  117 , out frequency:  21005.470085470086
	; 2nd TC:  21 , out frequency:  1000.2604802604803	
	; CH 1
	ld a, 0b01000111
	out CLK_CH1, a
	ld a, 117
	out CLK_CH1, a
	
	; CH 2
	ld a, 0b11000111
	out CLK_CH2, a
	ld a, 21
	out CLK_CH2, a

	ld hl, T_RAW_MILIS
	ld (hl), 0
	ld hl, T_MINUTES
	ld (hl), 0
	ld hl, T_SECONDS
	ld (hl), 0
	ld hl, T_MILIS
	ld bc, 0
	ld (hl), bc

	pop hl
	pop bc
	pop af
	ret


#data _BIOS_RAM
T_RAW_MILIS defs 2,0
T_MILIS defs 2,0
T_SECONDS defs 2,0
T_MINUTES defs 2,0

#code _BIOS_CODE

int_ctc_ch0:
	di
	ex af,af'
	exx

	ld hl, T_RAW_MILIS
	ld (hl), 0x11

	exx
	ex af,af'
	jr int_done

int_ctc_ch1:
	di
	ex af,af'
	exx

	ld hl, T_RAW_MILIS
	ld (hl), 0x22

	exx
	ex af,af'
	jr int_done

int_ctc_ch2:
	di
	ex af,af'
	exx
	
	; increment milliseconds
	ld hl, T_RAW_MILIS
	ld de, (hl)
	ld a, 1
	add a, e
	ld e, a
	adc a, d
	sub e
	ld d, a
	ld (hl), de

	ld hl, T_MILIS
	ld de, (hl)
	ld a, 1
	add a, e
	ld e, a
	adc a, d
	sub e
	ld d, a
	ld (hl), de


	;jr no_second_rollover 
	ld hl, 999
	and a
	sbc hl, de
	jr nc, no_second_rollover 
	

	ld hl, T_MILIS
	ld (hl),0
	inc hl
	ld (hl), 0

;	jr no_second_rollover
	
	ld hl, T_SECONDS
	inc (hl)
	ld e, (hl)
	ld d, 0
	ld hl, 59
	and a
	sbc hl, de
	jr nc, no_minute_rollover

	ld hl, T_SECONDS
	ld (hl), 0
	ld hl, T_MINUTES
	inc (hl)


no_minute_rollover:
no_second_rollover:

	; incements minutes

	exx
	ex af,af'
	jr int_done

int_ctc_ch3:
	di
	ex af,af'
	exx

	ld hl, T_RAW_MILIS
	ld (hl), 0x44

	exx
	ex af,af'

	jr int_done

int_done:
	ei
	reti

init_interrupts:
	ld a, 0 	; our mode2 interrupt handler is at 0x2090
	ld i, a
	im 2
	ei
	ret


