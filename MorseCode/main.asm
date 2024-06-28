;
; MorseCode.asm
;
; Created: 11/06/2024 17:57:39
; Author : Leya Wehner
; Group : K
;

; Size of the recive buffer for data send over UART
.equ BUFFER_SIZE = 255 

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
.equ DOT_CYCLES = CLK_FREQUENCY_IN_HZ / (1000. / DOT_TIME) / 10
.equ DASH_CYCLES = CLK_FREQUENCY_IN_HZ / (1000. / DASH_TIME) / 10
.equ CHAR_SPACE_CYCLES = CLK_FREQUENCY_IN_HZ / (1000. / CHAR_SPACE) / 10
.equ LETTER_SPACE_CYCLES = CLK_FREQUENCY_IN_HZ / (1000. / LETTER_SPACE) / 10
.equ WORD_SPACE_CYCLES = CLK_FREQUENCY_IN_HZ / (1000. / WORD_SPACE) / 10

; More meaningful names for registers
.def BUF_INDEX = r16
.def CHAR = r17
.def MORSE_CODE = r18
.def CODE_LEN= r19
.def BITMASK = r20
.def TEMP = r21

.def CNT_LOW = r22
.def CNT_MID = r23
.def CNT_HIGH = r24
.def ZERO_CHECK = r25

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
	ldi TEMP, 207 ; 103 with U2X0 unset
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
	sbi TRANSMITTER_DDR, TRANSMITTER_PIN ; Configure LED pin as output
	clr BUF_INDEX
main_loop:
	rcall send_acknowledge ; Send acknowledge over UART to signify the device is ready to recive the data
	rcall read_to_buffer ; Read data from UART into buffer in memory

	ldi ZL, LOW(buffer_start)
	ldi ZH, HIGH(buffer_start)

	blink_loop:
		ld CHAR, Z+ ; Load next char in CHAR register and post increment Z to yield the subsequent char on the next iteration
		
		cpi CHAR, 0 ; Test if CHAR is zero...
		breq main_loop ; ...and if so end of string is reached and we branch accordingly
		
		cpi CHAR, 13 ; Alternatively test if CHAR is carriage return (send on hitting enter by PuTTY)...
		breq main_loop ; ...and if so end of string is also reached
	
		cpi CHAR, ' '
		brne handle_char
	
		; Case char is space
		space_word
		rjmp blink_loop

		; Case char is a normal character
		handle_char:
			rcall map_char_to_morse
			rcall transmit_code
			space_letter
			rjmp blink_loop

; Function that reads the UART input into a memory buffer located at the buffer_start address, and echos recived characters back to the terminal
read_to_buffer:
	push TEMP
	in TEMP, SREG
	push TEMP
	push ZL
	push ZH
	push CHAR
	push BUF_INDEX
	push ZERO_CHECK

	clr ZERO_CHECK

	wait_for_data:
		lds TEMP, UCSR0A
		sbrs TEMP, RXC0
		rjmp wait_for_data

	lds CHAR, UDR0

	cpi CHAR, 8 ; Char is BS
	breq handle_backspace

	cpi CHAR, 127 ; Char is DEL (Send by PuTTY on hitting backspace)
	breq handle_backspace
	
	ldi ZL, LOW(buffer_start)
	ldi ZH, HIGH(buffer_start)
	add ZL, BUF_INDEX
	adc ZH, ZERO_CHECK
	st Z, CHAR

	; Echo the caracter back to the terminal
	rcall wait_for_udre
	sts UDR0 , CHAR
	
	; If CHAR is 0 or the carriage return character the end of input has been reached
	cpi CHAR, 0
	breq end_read
	cpi CHAR, 13
	breq end_read

	inc BUF_INDEX

	; Check for buffer overflow
	cpi BUF_INDEX, BUFFER_SIZE
	brlo wait_for_data      ; If not overflowed, continue receiving
	
	; Handle buffer overflow (reset index)
	clr buf_index
	rjmp wait_for_data

	handle_backspace:
		cpi buf_index, 0
		breq echo_backspace
		dec buf_index

	echo_backspace:
		rcall wait_for_udre
		sts UDR0 , CHAR
		rjmp wait_for_data

	end_read:
		; Send CRLF
		rcall wait_for_udre
		ldi TEMP, 13
		sts UDR0, TEMP
		rcall wait_for_udre
		ldi TEMP, 10
		sts UDR0, TEMP

		pop ZERO_CHECK
		pop BUF_INDEX
		pop CHAR
		pop ZH
		pop ZL
		pop TEMP
		out SREG, TEMP
		pop TEMP
		ret

send_acknowledge:
	push TEMP
	in TEMP, SREG
	push TEMP
	push CHAR
	push ZL
	push ZH

	ldi ZL, LOW(ack_msg << 1)
	ldi ZH, HIGH(ack_msg << 1)

	ack_next_char:
		lpm CHAR, Z+ ; Load next char in CHAR register and post increment Z to yield the subsequent char on the next iteration
		tst CHAR ; Test if CHAR is zero or negative...
		breq end_ack ; ...and if so end of string is reached and we branch accordingly

		rcall wait_for_udre
		sts UDR0 , CHAR
		rjmp ack_next_char

	end_ack:
		pop ZH
		pop ZL
		pop CHAR
		pop TEMP
		out SREG, TEMP
		pop TEMP
		ret

; Transmits the morse code sequence over the GPIO as defined in TRANSMITTER_PORT and TRANSMITTER_DDR
; Params:
;	MORSE_CODE: The morse code sequence to transmit
;	CODE_LEN: The lenght of the morse code to transmit
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
		rjmp prepare_next

	dash:
		transmit_dash
	
	prepare_next:
		; Check if the next bit in the MORSE_CODE register is still part of the Morse code
		dec CODE_LEN ; Decrement CODE_LEN
		tst CODE_LEN ; Check CODE_LEN for zero
		breq end_transmit ; Jump to end if all the bits that were part of the code are transmited

		lsr BITMASK ; Shift BITMASK to the right to look at the next bit in the subsequent iteration

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

; Maps a given character to it's coresponding morse code sequence
; Params:
;	CHAR: The char that should be mapped to morse code
; Returns:
;	MORSE_CODE: The morse code sequence representing the given char
;	CODE_LEN: The lenght of the morse code frequence
map_char_to_morse:
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
	
	pop CHAR
	pop TEMP
	out SREG, TEMP
	pop TEMP
	pop ZH
	pop ZL
	
	ret

wait_for_udre:
	push TEMP
	in TEMP, SREG
	push TEMP

	udre_loop:
		lds TEMP, UCSR0A
		sbrs TEMP, UDRE0
		rjmp udre_loop

	pop TEMP
	out SREG, TEMP
	pop TEMP
	ret

delay:
	; decrement counter :
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

ack_msg:
    .db "OK!", 13, 10, 0

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

.dseg
buffer_start:
    .byte BUFFER_SIZE   ; Define buffer in SRAM
