ClearDisplay equ 0xf5
StringOut equ 0xc2

#target bin
#code _CODE, 0x2082, 0x100	; 0x882 is the allocated memory for application of size 0x6fe (2 bytes reserved for app size)
#code _CODE

	jr main

TXT .asciz "Hello Yall!"

main:
	call ClearDisplay
	ld hl, TXT
	call StringOut

stop:
	jr stop