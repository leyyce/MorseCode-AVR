;
; MorseCode.asm
;
; Created: 11/06/2024 17:57:39
; Author : Leya Wehner
;

; Define the LED pin (assuming it's connected to port D)
.equ LED_PIN = 2

; Clock frequency in Hz
.equ CLK_FREQUENCY_IN_HZ = 16000000

; Define timing constants (in milliseconds)
.equ DOT_TIME = 100  ; Duration of a dot
.equ DASH_TIME = 300 ; Duration of a dash
.equ CHAR_SPACE = 100 ; Space between parts of the same letter
.equ LETTER_SPACE = 300 ; Space between letters
.equ WORD_SPACE = 700 ; Space between words

.equ DOT_CYCLES = 0.5 / (1000. / DOT_TIME) * 2 / (1. / clk_frequency_in_Hz) / 11
.equ DASH_CYCLES = 0.5 / (1000. / DASH_TIME) * 2 / (1. / clk_frequency_in_Hz) / 11
.equ CHAR_SPACE_CYCLES = 0.5 / (1000. / CHAR_SPACE) * 2 / (1. / clk_frequency_in_Hz) / 11
.equ LETTER_SPACE_CYCLES = 0.5 / (1000. / LETTER_SPACE) * 2 / (1. / clk_frequency_in_Hz) / 11
.equ WORD_SPACE_CYCLES = 0.5 / (1000. / WORD_SPACE) * 2 / (1. / clk_frequency_in_Hz) / 11

.def CHAR = r16
.def MORSE_CODE = r17
.def CODE_LEN= r18
.def TEMP = r19
.def TEMP2 = r20

.def CNT_LOW = r21
.def CNT_MID = r22
.def CNT_HIGH = r23
.def ZERO_CHECK = r24

.macro load_cycles
	ldi CNT_LOW, byte1(@0)
	ldi CNT_MID, byte2(@0)
	ldi CNT_HIGH, byte3(@0)
.endmacro

.macro wait_char
	load_cycles CHAR_SPACE_CYCLES
	rcall delay_long
.endmacro

.macro wait_letter
	load_cycles LETTER_SPACE_CYCLES
	rcall delay_long
.endmacro

.macro wait_word
	load_cycles WORD_SPACE_CYCLES
	rcall delay_long
.endmacro

.macro blink_dot
	load_cycles DOT_CYCLES
	sbi PORTD, LED_PIN
	rcall delay_long
	cbi PORTD, LED_PIN
.endmacro

.macro blink_dash
	load_cycles DASH_CYCLES
	sbi PORTD, LED_PIN
	rcall delay_long
	cbi PORTD, LED_PIN
.endmacro

start:
	; Configure LED pin as output
	sbi DDRB, LED_PIN
	
	; Load string address
	ldi ZL, LOW(string << 1)
	ldi ZH, HIGH(string << 1)
	
	; Main loop
	main_loop:
	lpm CHAR, Z+
	tst CHAR
	breq end ; End of string
	
	; Convert character to Morse code
	rcall map_char_to_morse
	
	; Blink the LED according to Morse code
	rcall blink_code
	
	wait_letter
	
	rjmp main_loop
	
	end:
	rjmp end

blink_code:
	ldi TEMP, 0b10000000
	
	morse_loop:
	mov TEMP2, MORSE_CODE
	and TEMP2, TEMP
	tst TEMP2
	brne dash
	dot:
	blink_dot
	rjmp skip_dash
	dash:
	blink_dash
	
	skip_dash:
	LSR TEMP

	clr TEMP2
	dec CODE_LEN
	cpse CODE_LEN, TEMP2
	wait_char
	tst CODE_LEN
	brne morse_loop
	ret

map_char_to_morse:
	push ZL
	push ZH
	
	; Convert ASCII character to table index
	; Assumes input is uppercase letter ('A' = 0, 'B' = 1, ...)
	subi CHAR, 'A'  ; Convert ASCII to 0-based index
	add CHAR, CHAR
	
	; Load address of Morse code table
	ldi ZL, LOW(morse_table << 1)
	ldi ZH, HIGH(morse_table << 1)
	
	; Fetch Morse code from table
	clr TEMP
	add r30, CHAR
	adc r31, TEMP
	lpm MORSE_CODE, Z+  ; Load Morse code into MORSE_CODE register
	lpm CODE_LEN, Z  ; Load code lengt into CODE_LEN register
	
	pop ZH
	pop ZL
	
	ret

delay_long:
	; decrement counter :
	clc ; clear carry | 1 cycle
	sbci CNT_LOW, 1 ; subtract low byte | 1 cycle
	sbci CNT_MID, 0 ; subtract mid byte | 1 cycle
	sbci CNT_HIGH, 0 ; subtract high byte | 1 cycle

	; check if counter == 0
	clr ZERO_CHECK ; 1 cycle
	or ZERO_CHECK, CNT_LOW ; 1 cycle
	or ZERO_CHECK, CNT_MID ; 1 cycle
	or ZERO_CHECK, CNT_HIGH ; 1 cycle
	tst ZERO_CHECK ; 1 cycle
	brne delay_long ; continue when cnt_high != 0 | 2 cycles
	ret

; String to be converted to Morse code
.org 0x0100
string:
    .db "TESTSTRING", 0

.org 0x0200
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
