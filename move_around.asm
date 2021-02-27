
ClearDisplay equ 0xf7
PIOOut equ 0x054F
ApplicationReturn equ 0x348
; set caret anywhere on the screen
; b - offset, 64 (0x40) is 2nd line
; c - offset on the line
SetCaretRaw_bios equ 0x11C

; PIO direct access
DSP_IW	equ	0b00000001
DSP_IR	equ 0b00000011
DSP_IRC	equ 0b00000010
DSP_DW	equ	0x5
PORTA 	equ 0x4
PORTB 	equ 0x5
PORTAC 	equ 0x6
PORTBC 	equ 0x7

OutHex equ $047B
NumToHex equ $0292
DisableCursor equ $0497
Wait_PS2ScanCode equ $04F8
; there are RAM mapped
STATE_KEYSTROKE equ $083E
STATE_SCANCODE equ $0842
Wait equ 0x60



#target bin
#code _CODE, 0x2082, 0x6fe	; 0x882 is the allocated memory for application of size 0x6fe (2 bytes reserved for app size)
#code _CODE

; app entry point
	jp main

xpos defb 0
ypos defb 0


moveup:
	ld hl, ypos
	ld a, (hl)
	or 0
	ret z
	dec (hl)
	ret

movedown:
	ld hl, ypos
	ld a, (hl)
	sub 16
	ret z
	inc (hl)
	ret

moveleft:
	ld hl, xpos
	ld a, (hl)
	or 0
	ret z
	dec (hl)
	ret

moveright:
	ld hl, xpos
	ld a, (hl)
	sub 10
	ret z
	inc (hl)
	ret
 
reset_2nd_line:
	push bc
	ld b, 64
	ld c, 0
	call SetCaretRaw
	pop bc
	ret

output_keycode:
	push bc
	push de
	push hl
#local
	; output currently pressed key code on the 2nd line of lcd
	ld b, 64
	ld c, 0
	call SetCaretRaw

	ld hl, STATE_KEYSTROKE
	; quick check if we have any keystroke chars
	ld a, (hl)
	or 0
	jp z, done
	ld b, a
	inc hl
outcode:	
	ld a, (hl)
	call OutHex
	inc hl
	djnz outcode

done:	
#endlocal
	; transform A register to d & e codes
	pop hl
	pop de
	pop bc
	ret

move:
#local
	ld a, (STATE_KEYSTROKE + 1)

	push af
	sub 6bh
	call z, moveleft
	pop af

	push af
	sub 74h
	call z, moveright
	pop af

	push af
	sub 75h
	call z, moveup
	pop af

	push af
	sub 72h
	call z, movedown
	pop af

#endlocal
	ret

reset_pos:
	push hl
	ld hl, xpos
	ld (hl), 0
	ld hl, ypos
	ld (hl), 0
	pop hl
	ret

draw_player:
	push bc
	push hl
	push de
#local
	ld a, (ypos)
	ld b, a
	ld a, 0
moverow:
	add a, 64
	djnz moverow
	ld b, a
	
	ld hl, xpos
	ld c, (hl)

	call SetCaretRaw

	ld e, '+'
	ld d, DSP_DW
	call PIOOut

#endlocal
	pop de
	pop hl
	pop bc
	ret

draw_player_location:
	ld b, 64
	ld c, 11
	call SetCaretRaw

	ld d, DSP_DW
	ld e, '['
	call PIOOut

	ld a, (xpos)
	call OutHex

	ld e, ','
	call PIOOut

	ld a, (ypos)
	call OutHex

	ld d, DSP_DW
	ld e, ']'
	call PIOOut

	ret	

main:
	ld hl, STATE_KEYSTROKE
	ld (hl), 0
	call ClearDisplay
	call reset_pos
	call DisableCursor
	call write_player_chars_to_cgram
	call write_boundaries_chars

	; render
	call draw_field

runloop:

	;call cleardisplay_fast
	;call ClearDisplay

	; call output_keycode

	call draw_player_location

	; call draw_player

	; wait for input
	call Wait_PS2ScanCode
	call scancode_to_keystroke

	; move
	call move
	call update_field_chars

	jr runloop

exit:
	;halt
	jp ApplicationReturn


draw_field:
	; set caret anywhere on the screen
	; b - offset, 64 (0x40) is 2nd line
	; c - offset on the line
	; call SetCaretRaw after
	; 
	; move cursor to the fields entry
	ld b, 0
	ld c, 5
	call SetCaretRaw

	ld d, DSP_DW

	ld e, 4
	call PIOOut

	ld e, 0
	call PIOOut

	ld e, 1
	call PIOOut

	ld e, 5
	call PIOOut


	; second line
	ld b, 64
	ld c, 5
	call SetCaretRaw

	ld d, DSP_DW

	ld e, 4
	call PIOOut
	ld e, 2
	call PIOOut
	ld e, 3
	call PIOOut
	ld e, 5
	call PIOOut

	ret

LEFT_EDGE equ 0b01100000
RIGHT_EDGE equ 0b01101000

FIELD_TOP_LEFT equ 0b01000000
FIELD_TOP_RIGHT equ 0b01001000
FIELD_BOTTOM_LEFT equ 0b01010000
FIELD_BOTTOM_RIGHT equ 0b01011000

EMPTY_CHAR 	defb	0b00000000
			defb	0b00000000
			defb	0b00000000
			defb	0b00000000
			defb	0b00000000
			defb	0b00000000
			defb	0b00000000
			defb	0b00000000

LEFT_EDGE_CHAR 	defb	0b00000001
				defb	0b00000001
				defb	0b00000001
				defb	0b00000001
				defb	0b00000001
				defb	0b00000001
				defb	0b00000001
				defb	0b00000001

RIGHT_EDGE_CHAR 	defb	0b00010000
					defb	0b00010000
					defb	0b00010000
					defb	0b00010000
					defb	0b00010000
					defb	0b00010000
					defb	0b00010000
					defb	0b00010000

CUSTOM_1_CHAR 	defb	0b00000000
				defb	0b00000000
				defb	0b00000100
				defb	0b00001100
				defb	0b00000100
				defb	0b00000100
				defb	0b00001110
				defb	0b00000000

CUSTOM_3_CHAR 	defb	0b00000000
				defb	0b00000000
				defb	0b00001110
				defb	0b00000010
				defb	0b00001110
				defb	0b00000010
				defb	0b00001110
				defb	0b00000000

CUSTOM_2_CHAR 	defb	0b00000000
				defb	0b00000000
				defb	0b00001110
				defb	0b00000010
				defb	0b00001110
				defb	0b00001000
				defb	0b00001110
				defb	0b00000000


CUSTOM_4_CHAR 	defb	0b00000000
				defb	0b00000000
				defb	0b00001010
				defb	0b00001010
				defb	0b00001110
				defb	0b00000010
				defb	0b00000010
				defb	0b00000000


PLAYAREA defs 11 * 17, 3


write_write_char_to_cgram:
	ld d, DSP_DW
	ld b, 8
#local
nextchar:
	ld e, (hl)
	call PIOOut
	inc hl
	djnz nextchar
#endlocal
	ret

write_player_chars_to_cgram:
	push de
	; CGRAM address is b0000*000..b0000*111 is custom chars
	; that gives us 0x00..0x7, so 8 custom characters in CGRAM
	; d is control address
	; e is data

	; figure out the char to send

	; BRUTE FORCE PIXEL PUSH
	; Characeter top/left
	ld d, DSP_IW
	ld e, FIELD_TOP_LEFT
	call PIOOut

	ld hl, CUSTOM_1_CHAR
	call write_write_char_to_cgram

	; top right
	ld d, DSP_IW
	ld e, FIELD_TOP_RIGHT
	call PIOOut

	ld hl, CUSTOM_2_CHAR
	call write_write_char_to_cgram

	; bottom left
	ld d, DSP_IW
	ld e, FIELD_BOTTOM_LEFT
	call PIOOut

	ld hl, CUSTOM_3_CHAR
	call write_write_char_to_cgram

	; bottom right
	ld d, DSP_IW
	ld e, FIELD_BOTTOM_RIGHT
	call PIOOut

	ld hl, CUSTOM_4_CHAR
	call write_write_char_to_cgram

	pop de
	ret


clear_char:
	push de
	ld hl, EMPTY_CHAR
	call write_write_char_to_cgram
	pop de
	ret

update_field_chars:
	push de
	push bc
#local
	; clear out playground

	ld hl, EMPTY_CHAR
	ld de, CUSTOM_1_CHAR
	ld bc, 8
	ldir

	ld hl, EMPTY_CHAR
	ld de, CUSTOM_2_CHAR
	ld bc, 8
	ldir

	ld hl, EMPTY_CHAR
	ld de, CUSTOM_3_CHAR
	ld bc, 8
	ldir

	ld hl, EMPTY_CHAR
	ld de, CUSTOM_4_CHAR
	ld bc, 8
	ldir

	; try to light up the player dot
	; figure out which quadrant it belongs to

	; check invisible pixels
	ld a, (xpos)
	cp 5
	jp z, nothing_to_do_here

	ld a, (ypos)
	cp 8
	jp z, nothing_to_do_here

	ld a, (xpos)
	cp 5
	; C is set -> char1 or char3 (we use empty pixel offset to avoid checking Z flag too)
	; => char 2 or char4 otherwise
	jr nc, second_col
	ld hl, CUSTOM_1_CHAR
	ld b, a
	jr col_set
second_col:	
	ld hl, CUSTOM_2_CHAR
	sub a, 6
	ld b, a
col_set:
	
	push bc
	push de
	ld a, (ypos)
	cp 0
	jr z, row_offset_calculated

	ld b, a
	cp 8
	jr z, row_offset_calculated
	jr c, next_row
	dec b
next_row:
	inc hl
	djnz next_row

row_offset_calculated:
	pop de
	pop bc

	ld a, b
	or 0
	ld a, 0b00010000
	jr z, zero_offset
shift_char_hor:
	sra a
	djnz shift_char_hor
zero_offset:

	ld (hl), a

nothing_to_do_here:
	call write_player_chars_to_cgram

#endlocal
	pop bc
	pop de
	ret

write_boundaries_chars:
	push de
	; boundaries
	; left
	ld d, DSP_IW
	ld e, LEFT_EDGE
	call PIOOut

	ld hl, LEFT_EDGE_CHAR
	call write_write_char_to_cgram

	; right
	ld d, DSP_IW
	ld e, RIGHT_EDGE
	call PIOOut

	ld hl, RIGHT_EDGE_CHAR
	call write_write_char_to_cgram

	pop de
	ret

scancode_to_keystroke:
	push hl
	push de
	push bc

	; test only => copy pure scancode into keystroke
	ld de, STATE_KEYSTROKE
	ld hl, STATE_SCANCODE
	ld b, 0
	ld c, 4
	ldir

	pop bc
	pop de
	pop hl
	ret


cleardisplay_fast:
	; clear display
	ld d, DSP_IW
	ld e, 0b00000001
	call PIOOut
	ret

SetCaretRaw:
	push af

	; b - offset, 64 (0x40) is 2nd line
	; c - offset on the line
	ld d, DSP_IW
	ld a, 0x80
	add b
	add c
	ld e, a

	call PIOOut

	pop af
	ret

