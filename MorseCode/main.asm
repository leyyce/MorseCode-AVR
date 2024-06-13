;
; MorseCode.asm
;
; Created: 11/06/2024 17:57:39
; Author : Leya Wehner
;

; Define the port register the transmitter is connected to
.equ TRANSMITTER_PORT = PORTD
; Defines the data direction register the transmitter is connected to
.equ TRANSMITTER_DDR = DDRD
; Define the pin the transmitter is connected to
.equ TRANSMITTER_PIN = 2

; Clock frequency in Hz
.equ CLK_FREQUENCY_IN_HZ = 16000000

; Define timing constants (in milliseconds)
.equ DOT_TIME = 200  ; Duration of a dot
.equ DASH_TIME = 3 * DOT_TIME ; Duration of a dash
.equ CHAR_SPACE = DOT_TIME ; Space between parts of the same letter
.equ LETTER_SPACE = DASH_TIME ; Space between letters
.equ WORD_SPACE = 7 * DOT_TIME ; Space between words

; Define amount of clock cycles to yield the desired wait times
.equ DOT_CYCLES = (DOT_TIME / 1000.) / (1. / clk_frequency_in_Hz) / 11
.equ DASH_CYCLES = (DASH_TIME / 1000.) / (1. / clk_frequency_in_Hz) / 11
.equ CHAR_SPACE_CYCLES = (CHAR_SPACE / 1000.) / (1. / clk_frequency_in_Hz) / 11
.equ LETTER_SPACE_CYCLES = (LETTER_SPACE / 1000.) / (1. / clk_frequency_in_Hz) / 11
.equ WORD_SPACE_CYCLES = (WORD_SPACE / 1000.) / (1. / clk_frequency_in_Hz) / 11

; More meaningful names for registers
.def CHAR = r16
.def MORSE_CODE = r17
.def CODE_LEN= r18
.def BITMASK = r19
.def TEMP = r20

.def CNT_LOW = r21
.def CNT_MID = r22
.def CNT_HIGH = r23
.def ZERO_CHECK = r24

; Macro that loads the curently needed clock cycle count into the coresponding register
; Params:
;	@0: The amount of clock cycles to load 
.macro load_cycles
	ldi CNT_LOW, byte1(@0)
	ldi CNT_MID, byte2(@0)
	ldi CNT_HIGH, byte3(@0)
.endmacro

; Macro to leave a space between parts of the same letter
.macro space_char
	load_cycles CHAR_SPACE_CYCLES
	rcall delay
.endmacro

; Macro to leave a space between letters
.macro space_letter
	load_cycles LETTER_SPACE_CYCLES
	rcall delay
.endmacro

; Macro to leave a space between words
.macro space_word
	load_cycles WORD_SPACE_CYCLES
	rcall delay
.endmacro

; Macro to transmit a dot
.macro transmit_dot
	load_cycles DOT_CYCLES
	sbi TRANSMITTER_PORT, TRANSMITTER_PIN
	rcall delay
	cbi TRANSMITTER_PORT, TRANSMITTER_PIN
.endmacro

; Macro to transmit a dash
.macro transmit_dash
	load_cycles DASH_CYCLES
	sbi TRANSMITTER_PORT, TRANSMITTER_PIN
	rcall delay
	cbi TRANSMITTER_PORT, TRANSMITTER_PIN
.endmacro

.macro init_uart
	ldi TEMP, 207 ; 103 with TXEN0 unset
	sts UBRR0L, TEMP
	clr TEMP
	sts UBRR0H, TEMP
	lds TEMP, UCSR0A
	ori TEMP, (1 << U2X0)
	sts UCSR0A, TEMP
	ldi TEMP, (1 << RXEN0) | (1 << TXEN0)
	sts UCSR0B, TEMP
.endmacro

init:
	init_uart
	; Configure LED pin as output
	sbi TRANSMITTER_DDR, TRANSMITTER_PIN
main_loop:
	lds TEMP, UCSR0A
	andi TEMP, (1 << RXC0)
	tst TEMP
	breq main_loop
	lds CHAR, UDR0

	cpi CHAR, 32 ; Check if char is space
	brne handle_char
	
	; Case char is space
	space_word
	rjmp main_loop

	; Case char is a normal character
	handle_char:
		rcall map_char_to_morse
		rcall transmit_code
		space_letter
		rjmp main_loop

transmit_code:
	push TEMP
	in TEMP, SREG
	push TEMP
	push BITMASK
	push MORSE_CODE
	push CODE_LEN


	ldi BITMASK, 0b10000000 ; Bit mask to check needed bit in MORSE_CODE register
	
	morse_loop:
		mov TEMP, MORSE_CODE ; Copy MORSE_CODE in TEMP register
		and TEMP, BITMASK ; Bitwise AND with BITMASK to look only at the needed bit 
		tst TEMP ; Test if bit is zero
		brne dash ; If bit is not zero we transmit a dash. If it is zero we transmit a dot

	dot:
		transmit_dot
		rjmp skip_dash

	dash:
		transmit_dash
	
	skip_dash:
		lsr BITMASK ; Shift BITMASK to the right to look at the next bit in the subsequent iteration

		; Check if the next bit in the MORSE_CODE register is still part of the Morse code
		dec CODE_LEN ; Decrement CODE_LEN
		tst CODE_LEN ; Check CODE_LEN for zero
		breq end_transmit ; Jump to end if all the bits that were part of the code are transmited

		space_char
		rjmp morse_loop

	end_transmit:
		pop CODE_LEN
		pop MORSE_CODE
		pop BITMASK
		pop TEMP
		out SREG, TEMP
		pop TEMP
		ret

map_char_to_morse:
	; Save Z register to stack
	push ZL
	push ZH
	push TEMP
	in TEMP, SREG
	push TEMP
	push CHAR
	
	; Convert ASCII character to table index
	; Assumes input is lowercase letter ('a' = 0, 'b' = 1, ...)
	subi CHAR, 'a' ; Convert ASCII to 0-based index
	add CHAR, CHAR ; Offset by the doubled amount as each Morse table entry for a letter consists of two bytes of data
	
	; Load address of Morse code table
	ldi ZL, LOW(morse_table << 1)
	ldi ZH, HIGH(morse_table << 1)
	
	; Fetch Morse code from table
	clr TEMP
	add ZL, CHAR
	adc ZH, TEMP
	lpm MORSE_CODE, Z+  ; Load Morse code into MORSE_CODE register
	lpm CODE_LEN, Z  ; Load code lengt into CODE_LEN register
	
	; Restore Z register from stack
	pop CHAR
	pop TEMP
	out SREG, TEMP
	pop TEMP
	pop ZH
	pop ZL
	
	ret

delay:
	; decrement counter :
	clc ; clear carry | 1 cycle
	subi CNT_LOW, 1 ; subtract low byte | 1 cycle
	sbci CNT_MID, 0 ; subtract mid byte | 1 cycle
	sbci CNT_HIGH, 0 ; subtract high byte | 1 cycle

	; check if counter == 0 in a constant amount of cycles
	clr ZERO_CHECK ; 1 cycle
	or ZERO_CHECK, CNT_LOW ; 1 cycle
	or ZERO_CHECK, CNT_MID ; 1 cycle
	or ZERO_CHECK, CNT_HIGH ; 1 cycle
	tst ZERO_CHECK ; 1 cycle
	brne delay ; continue when cnt_high != 0 | 2 cycles
	ret

; First element is the binary encoded sequence for the coresponding letter (starting at the leftmost bit)
;	0: Dot
;	1: Dash
; The second element is the lenght of the morse code so that we can ignore unused trailing zero bits
morse_table:
	.db 0b01000000, 2 ; A:	.-
	.db 0b10000000, 4 ; B:	-...
	.db 0b10100000, 4 ; C:	-.-.
	.db 0b10000000, 3 ; D:	-..
	.db 0b00000000, 1 ; E:	.
	.db 0b00100000, 4 ; F:	..-.
	.db 0b11000000, 3 ; G:	--.
	.db 0b00000000, 4 ; H:	....
	.db 0b00000000, 2 ; I:	..
	.db 0b01110000, 4 ; J:	.---
	.db 0b10100000, 3 ; K:	-.-
	.db 0b01000000, 4 ; L:	.-..
	.db 0b11000000, 2 ; M:	--
	.db 0b10000000, 2 ; N:	-.
	.db 0b11100000, 3 ; O:	---
	.db 0b01100000, 4 ; P:	.--.
	.db 0b11010000, 4 ; Q:	--.-
	.db 0b01000000, 3 ; R:	.-.
	.db 0b00000000, 3 ; S:	...
	.db 0b10000000, 1 ; T:	-
	.db 0b00100000, 3 ; U:	..-
	.db 0b00010000, 4 ; V:	...-
	.db 0b01100000, 3 ; W:	.--
	.db 0b10010000, 4 ; X:	-..-
	.db 0b10110000, 4 ; Y:	-.--
	.db 0b11000000, 4 ; Z:	--..
