; Members: Duc Nguyen, Yan Hong, Saad Ali, Aneet Lakhani, Divya Budihal, Alessandro Narcisso
; Project: Reflow Oven Controller
; NOTE: In this code, rftime is soak time, stime is reflow time, rftemp is soak temp, stemp is reflow temp 

$NOLIST
$MODLP52
$LIST

CLK           EQU 22118400 ; Microcontroller system crystal frequency in Hz
BAUD 		  equ 115200
T1LOAD 		  equ (0x100-(CLK/(16*BAUD)))
TIMER0_RATE   EQU 4096     ; 2048Hz squarewave (peak amplitude of CEM-1203 speaker)
TIMER0_RELOAD EQU ((65536-(CLK/TIMER0_RATE)))
TIMER2_RATE   EQU 1000     ; 1000Hz, for a timer tick of 1ms
TIMER2_RELOAD EQU ((65536-(CLK/TIMER2_RATE)))

UPTEN			  	equ P0.3
DOWNTEN				equ P0.4
BOOT_BUTTON   		equ P0.6
LED1				equ P0.7
LED2				equ P2.6
LED3				equ P4.4
LED4				equ P4.5
SOUND_OUT     		equ P2.7
MODE				equ P0.0
DOWN			 	equ P0.2
UP			     	equ P0.1
START		 	 	equ P2.4
RESET				equ P2.5

; Reset vector
org 0x0000
    ljmp main

; External interrupt 0 vector (not used in this code)
org 0x0003
	reti

; Timer/Counter 0 overflow interrupt vector
org 0x000B
  ljmp Timer0_ISR
  reti
    
    
; External interrupt 1 vector (not used in this code)
org 0x0013
	reti

; Timer/Counter 1 overflow interrupt vector (not used in this code)
org 0x001B
;	ljmp Timer1_ISR
	reti

; Serial port receive/transmit interrupt vector (not used in this code)
org 0x0023 
	reti
	
; Timer/Counter 2 overflow interrupt vector
org 0x002B
	ljmp Timer2_ISR
	
; In the 8051 we can define direct access variables starting at location 0x30 up to location 0x7F
dseg at 0x30
Count1ms:     	ds 3 ; Used to determine when half second has passed
rftime: 		ds 1
rftemp: 		ds 1
rftemp_compare: ds 2
stime:			ds 1
stemp:			ds 1
timer:  		ds 1
x	 :  		ds 4
y	 : 			ds 4
bcd  :			ds 5
modevar:		ds 1
change_state: 	ds 1
end_soak: 		ds 1
state:  		ds 1
pwm:  			ds 1
temp: 			ds 2
state_flag: 	ds 1
Result: 		ds 2
safety:			ds 1
trigger:		ds 1
safety_flag:	ds 1
destroyer:		ds 1
hundred:		ds 1

bseg
half_seconds_flag: dbit 1 ; Set to one in the ISR every time 500 ms had passed
rftime_flag: dbit 1 ; 
rftemp_flag: dbit 1 ;
stime_flag:  dbit 1 ;
stemp_flag:  dbit 1 ;
mf:			 dbit 1 ;
reset_flag:  dbit 1 ;

cseg
; These 'equ' must match the wiring between the microcontroller and the LCD!
;Pin config;
CE_ADC	EQU P2.0
MY_MOSI EQU P2.1
MY_MISO EQU P2.2
MY_SCLK EQU P2.3

LCD_RS equ P1.2
LCD_RW equ P1.3
LCD_E  equ P1.4
LCD_D4 equ P3.2
LCD_D5 equ P3.3
LCD_D6 equ P3.4
LCD_D7 equ P3.5
$NOLIST
$include(LCD_4bit.inc) ; A library of LCD related functions and utility macros
;$include(math32.inc) ; A library of LCD related functions and utility macros
$include(reflow_controller.inc) 
$LIST

;                     1234567890123456    <- This helps determine the location of the counter
TIME:    	      db 'Ss: 0   Rs: 0   ',0
TEMPTEXT:		  db 'Sc:     Rc:     ',0
PICK1:			  db '<',0
PICK2:			  db '>',0
CLEAR:			  db ' ',0
STAGESCREEN:	  db 'STAGE:          ',0
RFSCREEN:	  	  db 'Ss: 0   Sc:     ',0
SSCREEN:	  	  db 'Rs: 0   Rc:     ',0
CLEARSCREEN:	  db '                ',0
ERROR:			  db '     ERROR!     ',0

DONE:
	DB '-1\r\n',0
SOAK_INIT:
	DB '-2\r\n',0
REFLOW_INIT:
	DB '-3\r\n',0

;---------------------------------;
;        Wait 1.5 seconds         ;
;---------------------------------;
Wait2Sec:
    push AR0
    push AR1
    push AR3
    push AR4
    mov R4, #10
L7: mov R3, #50
L6: mov R1, #150
L5: mov R0, #150
L4: djnz R0, L4 ; 3 cycles->3*45.21123ns*150=20.3451us
    djnz R1, L5 ; 20.3451us*150=3.051758ms
    djnz R3, L6 ; 3.051758ms*50=0.1525879s
    djnz R4, L7 ; 0.1525879s*10=1.525879s
    pop AR4
    pop AR3
    pop AR1
    pop AR0
    ret
    
;---------------------------------;
;        Wait 7 seconds           ;
;---------------------------------;
Wait7Sec:
    push AR0
    push AR1
    push AR3
    push AR4
    mov R4, #46
L11: mov R3, #50
L10: mov R1, #150
L9: mov R0, #150
L8: djnz R0, L8 ; 3 cycles->3*45.21123ns*150=20.3451us
    djnz R1, L9 ; 20.3451us*150=3.051758ms
    djnz R3, L10 ; 3.051758ms*50=0.1525879s
    djnz R4, L11 ; 0.1525879s*46=7.0190434s
    pop AR4
    pop AR3
    pop AR1
    pop AR0
    ret

;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 0                     ;
;---------------------------------;
Timer0_Init:
	mov a, TMOD
	anl a, #0xf0 ; Clear the bits for timer 0
	orl a, #0x01 ; Configure timer 0 as 16-timer
	mov TMOD, a
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
;   Enable the timer and interrupts
;    setb ET0  ; Enable timer 0 interrupt
    setb TR0  ; Start timer 0
	ret

;---------------------------------;
; ISR for timer 0.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
Timer0_ISR:
	clr TF0  ; According to the data sheet this is done for us already.
	; In mode 1 we need to reload the timer.
	clr TR0
	mov TH0, #high(TIMER0_RELOAD)
	mov TL0, #low(TIMER0_RELOAD)
	setb TR0
	cpl SOUND_OUT ; Connect speaker to P3.7!
	reti
	
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 1                     ;
;---------------------------------;
;Timer1_Init:
;	mov TMOD, #10H
;	mov TH1, #high(TIMER0_RELOAD)
;	mov TL1, #low(TIMER0_RELOAD)
;	setb ET1
;	setb TR1
;	ret
;---------------------------------;
; ISR for timer 1.  Set to execute;
; every 1/4096Hz to generate a    ;
; 2048 Hz square wave at pin P3.7 ;
;---------------------------------;
;Timer1_ISR:
;	; In mode 1 we need to reload the timer.
;	clr TR1
;	mov TH1, #high(TIMER0_RELOAD)
;	mov TL1, #low(TIMER0_RELOAD)
;	setb TR1
;	cpl SOUND_OUT ; Connect speaker to P3.7!
;	reti
	
;---------------------------------;
; Routine to initialize the ISR   ;
; for timer 2                     ;
;---------------------------------;
Timer2_Init:
	mov T2CON, #0 ; Stop timer/counter.  Autoreload mode.
	mov RCAP2H, #high(TIMER2_RELOAD)
	mov RCAP2L, #low(TIMER2_RELOAD)
	orl T2MOD, #0x10
	mov T2EX, #0
	; Init One millisecond interrupt counter.  It is a 16-bit variable made with two 8-bit parts
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	; Enable the timer and interrupts
    setb ET2  ; Enable timer 2 interrupt
    setb TR2  ; Enable timer 2
	reti

;---------------------------------;
; ISR for timer 2                 ;
;---------------------------------;
Timer2_ISR:
	clr TF2  ; Timer 2 doesn't clear TF2 automatically. Do it in ISR
	cpl P3.6 ; To check the interrupt rate with oscilloscope. It must be precisely a 1 ms pulse.
	
	; The two registers used in the ISR must be saved in the stack
	push acc
	push psw
	
	; Increment the 16-bit one mili second counter
	inc Count1ms+0    ; Increment the low 8-bits first
	mov a, Count1ms+0 ; If the low 8-bits overflow, then increment high 8-bits
	jnz Inc_Done
	inc Count1ms+1
	
Inc_Done:
	; Check if half second has passed
    mov a, Count1ms+0
    cjne a, #low(1000), Timer2_ISR_done ; Warning: this instruction changes the carry flag!
    mov a, Count1ms+1
    cjne a, #high(1000), Timer2_ISR_done
	
	; 500 milliseconds have passed.  Set a flag so the main program knows
	setb half_seconds_flag ; Let the main program know a second had passed
;	cpl TR0 ; Enable/disable timer/counter 0. This line creates a beep-silence-beep-silence sound.
	; Reset to zero the milli-seconds counter, it is a 16-bit variable
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
	
	; Decrement the Value
	mov a, trigger
	cjne a, #1, stuck0
	clr a
	mov a, safety
	cjne a, #60, inc_safety
	clr a
	mov trigger, #0
	mov a, safety_flag
	cjne a, #0, stuck0
	mov destroyer, #1
	clr a
	sjmp stuck0
inc_safety:
	add a, #1
	mov safety, a 
	clr a
stuck0:
	mov a, state
	cjne a, #2, checkfour	
	
	mov a, rftime
	cjne a, #0x00, Next
	mov rftime, #0x00
	clr a 
checkfour:
	mov a, state
	cjne a, #4, Timer2_ISR_done	

	mov a, stime	
	cjne a, #0x00, Next2
	mov stime, #0x00
	sjmp Timer2_ISR_done
Next2:
	add a, #0x99
	da a
	mov stime,a
	sjmp Timer2_ISR_done
Next:
	add a, #0x99
	da a
	mov rftime, a
Timer2_ISR_done:

	pop psw
	pop acc
	reti
	
Save_Configuration_rf:
	; Erase FDATA page 1
	clr EA ; No interrupts please!
	mov MEMCON, #01011000B ; AERS=1, MWEN=1, DMEN=1
	mov DPTR, #0x0000
	mov a, #0xff
	movx @DPTR, A
	; Load page
	mov MEMCON, #00111000B ; LDPG=1, MWEN=1, DMEN=1
	; Save variables
	mov a, rftemp
	movx @DPTR, A
	inc DPTR	
	mov a, #0x55 ; First key value
	movx @DPTR, A
	inc DPTR	
	mov a, #0xAA ; Second key value
	movx @DPTR, A
	mov MEMCON, #00011000B ; Copy page to flash
	mov a, #0xff
	movx @DPTR, A
	mov MEMCON, #00000000B ; Disable further access to data flash
	setb EA ; Re-enable interrupts
	ret
	
Save_Configuration_s:
	; Erase FDATA page 1
	clr EA ; No interrupts please!
	mov MEMCON, #01011000B ; AERS=1, MWEN=1, DMEN=1
	mov DPTR, #0x0000
	mov a, #0xff
	movx @DPTR, A
	; Load page
	mov MEMCON, #00111000B ; LDPG=1, MWEN=1, DMEN=1
	; Save variables
	mov a, stemp
	movx @DPTR, A
	inc DPTR	
	mov a, #0x55 ; First key value
	movx @DPTR, A
	inc DPTR	
	mov a, #0xAA ; Second key value
	movx @DPTR, A
	mov MEMCON, #00011000B ; Copy page to flash
	mov a, #0xff
	movx @DPTR, A
	mov MEMCON, #00000000B ; Disable further access to data flash
	setb EA ; Re-enable interrupts
	ret

Load_Configuration_rf:
	mov MEMCON, #00001000B ; Enable read access to data flash
	mov dptr, #0x0001 ;First key value location.  Must be 0x55
	movx a, @dptr
	cjne a, #0x55, Load_Defaults
	inc dptr ; Second key value location.  Must be 0xaa
	movx a, @dptr
	cjne a, #0xaa, Load_Defaults
	; Keys are good.  Load saved values.
	mov dptr, #0x0000
	movx a, @dptr
	mov rftemp, a
	mov MEMCON, #00000000B ; Disable further access to data flash
	ret
	
Load_Configuration_s:
	mov MEMCON, #00001000B ; Enable read access to data flash
	mov dptr, #0x0001 ;First key value location.  Must be 0x55
	movx a, @dptr
	cjne a, #0x55, Load_Defaults
	inc dptr ; Second key value location.  Must be 0xaa
	movx a, @dptr
	cjne a, #0xaa, Load_Defaults
	; Keys are good.  Load saved values.
	mov dptr, #0x0000
	movx a, @dptr
	mov stemp, a
	mov MEMCON, #00000000B ; Disable further access to data flash
	ret

Load_Defaults: ; Load defaults if keys are incorrect
   mov rftemp, #014
   mov stemp, #014
   mov MEMCON, #00000000B ; Disable access to data flash
   ret

; Eight bit number to display passed in ‘a’.
Display_Accumulator:
	mov b, #100
	div ab
	orl a, #0x30 ; Convert hundreds to ASCII
	lcall ?WriteData ; display ASCII
	mov a, b    ; Remainder is in register b
	mov b, #10
	div ab
	orl a, #0x30 ; Convert tens to ASCII
	lcall ?WriteData ; display ASCII
	mov a, b
	orl a, #0x30 ; Convert units to ASCII
	lcall ?WriteData ; display ASCII
	ret
	
;---------------------------------;
; Main program. Includes hardware ;
; initialization and 'forever'    ;
; loop.                           ;
;---------------------------------;
main:
    
	; Initialization
    mov SP, #0x7F
    mov PMOD, #0 ; Configure all ports in bidirectional mode
    lcall Timer0_Init
;   lcall Timer1_Init
    lcall LCD_4BIT
    ; For convenience a few handy macros are included in 'LCD_4bit.inc':
    mov rftime, #0x10
	mov stime,  #0x10
	mov modevar, #0
	mov trigger, #0
	mov safety, #0
	mov safety_flag, #0
	mov destroyer, #0
    Set_Cursor(1, 1)
    Send_Constant_String(#TIME)
    Set_Cursor(2, 1)
    Send_Constant_String(#TEMPTEXT)
	Set_Cursor(1, 6)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(rftime) ; This macro is also in 'LCD_4bit.inc'
	Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
	Display_BCD(stime) ; This macro is also in 'LCD_4bit.inc'    
    setb half_seconds_flag
    mov change_state, #1
    mov end_soak, #1
    clr P2.6
    lcall InitSerialPort
	lcall INIT_SPI
    
	; display rc
    lcall Load_Configuration_rf
	Set_Cursor(2, 5)
	mov a, rftemp
	lcall Display_Accumulator
	; display sc
	lcall Load_Configuration_s
	Set_Cursor(2, 13)
	mov a, stemp
	lcall Display_Accumulator
	; After initialization the program stays in this 'forever' loop

;---------------------------------MAGIC-----------------------------------;
;---------------------------------HAPPENS---------------------------------;
;---------------------------------HERE------------------------------------;

check_mode:
;	clr ET0
	jb START, mode_choose  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb START, mode_choose  ; if the 'BOOT' button is not pressed skip
	jnb START, $		; Wait for button release.  The '$' means: jump to same instruction.
	ljmp start_infinity
mode_choose:
	clr ET0
	jb MODE, check_mode ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MODE, check_mode ; if the 'BOOT' button is not pressed skip
	jnb MODE, $		; Wait for button release.  The '$' means: jump to same instruction.
superloop:
	inc modevar
	clr a
startcheckingmode:
	mov a, modevar
	ljmp cont0
help1:
	ljmp cont1
cont0:
	cjne a, #1, help1
	; clear <> symbol
	Set_Cursor(1, 4)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 8)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 12)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 16)
	Send_Constant_String(#CLEAR)	
	Set_Cursor(2, 4)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 8)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 12)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 16)
	Send_Constant_String(#CLEAR)
	; set <> symbol
	Set_Cursor(1, 4)
	Send_Constant_String(#PICK1)
	Set_Cursor(1, 8)
	Send_Constant_String(#PICK2)
	ljmp check_rftime
help2:
	ljmp cont2
cont1:
	cjne a, #2, help2
	; clear <> symbol
	Set_Cursor(1, 4)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 8)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 12)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 16)
	Send_Constant_String(#CLEAR)	
	Set_Cursor(2, 4)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 8)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 12)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 16)
	Send_Constant_String(#CLEAR)
	; set <> symbol
	Set_Cursor(1, 12)
	Send_Constant_String(#PICK1)
	Set_Cursor(1, 16)
	Send_Constant_String(#PICK2)
	ljmp check_stime
help3:
	ljmp cont3
cont2:
	cjne a, #3, help3
	; clear <> symbol
	Set_Cursor(1, 4)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 8)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 12)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 16)
	Send_Constant_String(#CLEAR)	
	Set_Cursor(2, 4)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 8)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 12)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 16)
	Send_Constant_String(#CLEAR)
	; set <> symbol
	Set_Cursor(2, 4)
	Send_Constant_String(#PICK1)
	Set_Cursor(2, 8)
	Send_Constant_String(#PICK2)
	ljmp check_rftemp
help4:
	ljmp check_mode
cont3:
	cjne a, #4, help4
	mov modevar, #0
	; clear <> symbol
	Set_Cursor(1, 4)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 8)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 12)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 16)
	Send_Constant_String(#CLEAR)	
	Set_Cursor(2, 4)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 8)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 12)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 16)
	Send_Constant_String(#CLEAR)
	; set <> symbol
	Set_Cursor(2, 12)
	Send_Constant_String(#PICK1)
	Set_Cursor(2, 16)
	Send_Constant_String(#PICK2)
	ljmp check_stemp

;-------------------------------------RFTIME---------------------------------------------;
;-------------------------------------RFTIME---------------------------------------------;
;-------------------------------------RFTIME---------------------------------------------;	
check_rftime:
	jb UP, up_rftimeten  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb UP, up_rftimeten  ; if the 'BOOT' button is not pressed skip
	jnb UP, $		; Wait for button release.  The '$' means: jump to same instruction.
;UPrftime
	mov a, rftime
	add a, #0x01
	da a
	mov rftime, a
	Set_Cursor(1,6)
	Display_BCD(rftime)
	Set_Cursor(1,6)
	Display_BCD(rftime)
	ljmp check_rest_rftime
	
up_rftimeten:
	jb UPTEN, down_rftime  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb UPTEN, down_rftime  ; if the 'BOOT' button is not pressed skip
	jnb UPTEN, $		; Wait for button release.  The '$' means: jump to same instruction.
	;UPrftime
	mov a, rftime
	add a, #0x10
	da a
	mov rftime, a
	Set_Cursor(1,6)
	Display_BCD(rftime)
	Set_Cursor(1,6)
	Display_BCD(rftime)
	ljmp check_rest_rftime
	
down_rftime:
	jb DOWN, down_rftimeten  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb DOWN, down_rftimeten  ; if the 'BOOT' button is not pressed skip
	jnb DOWN, $		; Wait for button release.  The '$' means: jump to same instruction.
	mov a, rftime
	add a, #0x99
	da a
	mov rftime, a
	Set_Cursor(1,6)
	Display_BCD(rftime)
	ljmp check_rest_rftime
	
down_rftimeten:
	jb DOWNTEN, check_rest_rftime  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb DOWNTEN, check_rest_rftime  ; if the 'BOOT' button is not pressed skip
	jnb DOWNTEN, $		; Wait for button release.  The '$' means: jump to same instruction.
	mov a, rftime
	add a, #0x90
	da a
	mov rftime, a
	Set_Cursor(1,6)
	Display_BCD(rftime)

check_rest_rftime:
	jb MODE, check_rftime_middle  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MODE, check_rftime_middle  ; if the 'BOOT' button is not pressed skip
	jnb MODE, $		; Wait for button release.  The '$' means: jump to same instruction.
	ljmp check_mode
check_rftime_middle:
	ljmp check_rftime

;-------------------------------------STIME---------------------------------------------;
;-------------------------------------STIME---------------------------------------------;
;-------------------------------------STIME---------------------------------------------;
check_stime:
	jb UP, up_stime10  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb UP, up_stime10  ; if the 'BOOT' button is not pressed skip
	jnb UP, $		; Wait for button release.  The '$' means: jump to same instruction.
;UPrftime
	mov a, stime
	add a, #0x01
	da a
	mov stime, a
	Set_Cursor(1,14)
	Display_BCD(stime)
	ljmp check_rest_stime
	
up_stime10:
	jb UPTEN, down_stime  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb UPTEN, down_stime  ; if the 'BOOT' button is not pressed skip
	jnb UP, $		; Wait for button release.  The '$' means: jump to same instruction.
;UPrftime
	mov a, stime
	add a, #0x10
	da a
	mov stime, a
	Set_Cursor(1,14)
	Display_BCD(stime)
	Set_Cursor(1,14)
	Display_BCD(stime)
	ljmp check_rest_stime
	
down_stime:
	jb DOWN, down_stime10  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb DOWN, down_stime10  ; if the 'BOOT' button is not pressed skip
	jnb DOWN, $		; Wait for button release.  The '$' means: jump to same instruction.
	mov a, stime
	add a, #0x99
	da a
	mov stime, a
	Set_Cursor(1,14)
	Display_BCD(stime)
	ljmp check_rest_stime
	
down_stime10:
	jb DOWNTEN, check_rest_stime  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb DOWNTEN, check_rest_stime  ; if the 'BOOT' button is not pressed skip
	jnb DOWNTEN, $		; Wait for button release.  The '$' means: jump to same instruction.
	mov a, stime
	add a, #0x90
	da a
	mov stime, a
	Set_Cursor(1,14)
	mov stime, a
	Set_Cursor(1,14)
	Display_BCD(stime)

check_rest_stime:
	jb MODE, check_stime_middle  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MODE, check_stime_middle  ; if the 'BOOT' button is not pressed skip
	jnb MODE, $		; Wait for button release.  The '$' means: jump to same instruction.
	ljmp check_mode
check_stime_middle:
	ljmp check_stime
	
;-------------------------------------RFTEMP---------------------------------------------;
;-------------------------------------RFTEMP---------------------------------------------;
;-------------------------------------RFTEMP---------------------------------------------;
	
check_rftemp:
	jb UP, up_rftemp10  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb UP, up_rftemp10  ; if the 'BOOT' button is not pressed skip
	jnb UP, $		; Wait for button release.  The '$' means: jump to same instruction.
	inc rftemp
	Set_Cursor(2, 5)
	mov a, rftemp
	lcall Display_Accumulator
	lcall Save_Configuration_rf
	clr a
	ljmp check_rest_rftemp

up_rftemp10:	
	jb UPTEN, down_rftemp  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb UPTEN, down_rftemp  ; if the 'BOOT' button is not pressed skip
	jnb UPTEN, $		; Wait for button release.  The '$' means: jump to same instruction.
	inc rftemp
	inc rftemp
	inc rftemp
	inc rftemp
	inc rftemp
	inc rftemp
	inc rftemp
	inc rftemp
	inc rftemp
	inc rftemp
	Set_Cursor(2, 5)
	mov a, rftemp
	lcall Display_Accumulator
	lcall Save_Configuration_rf
	clr a
	ljmp check_rest_rftemp

down_rftemp:
	jb DOWN, down_rftemp10  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb DOWN, down_rftemp10  ; if the 'BOOT' button is not pressed skip
	jnb DOWN, $		; Wait for button release.  The '$' means: jump to same instruction.
	dec rftemp
	Set_Cursor(2, 5)
	mov a, rftemp
	lcall Display_Accumulator
	lcall Save_Configuration_rf
	clr a
	ljmp check_rest_rftemp
	
down_rftemp10:
	jb DOWNTEN, check_rest_rftemp  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb DOWNTEN, check_rest_rftemp  ; if the 'BOOT' button is not pressed skip
	jnb DOWNTEN, $		; Wait for button release.  The '$' means: jump to same instruction.
	dec rftemp
	dec rftemp
	dec rftemp
	dec rftemp
	dec rftemp
	dec rftemp
	dec rftemp
	dec rftemp
	dec rftemp
	dec rftemp
	Set_Cursor(2, 5)
	mov a, rftemp
	lcall Display_Accumulator
	lcall Save_Configuration_rf
	clr a

check_rest_rftemp:
	jb MODE, check_rftemp_middle  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MODE, check_rftemp_middle  ; if the 'BOOT' button is not pressed skip
	jnb MODE, $		; Wait for button release.  The '$' means: jump to same instruction.
	ljmp check_mode
	
check_rftemp_middle:
	ljmp check_rftemp
	
;-------------------------------------STEMP---------------------------------------------;
;-------------------------------------STEMP---------------------------------------------;
;-------------------------------------STEMP---------------------------------------------;
	
check_stemp:
	jb UP, up_stemp10  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb UP, up_stemp10  ; if the 'BOOT' button is not pressed skip
	jnb UP, $		; Wait for button release.  The '$' means: jump to same instruction.
	inc stemp
	Set_Cursor(2, 13)
	mov a, stemp
	lcall Display_Accumulator
	lcall Save_Configuration_s
	clr a
	ljmp check_rest_stemp

up_stemp10:	
	jb UPTEN, down_stemp  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb UPTEN, down_stemp  ; if the 'BOOT' button is not pressed skip
	jnb UPTEN, $		; Wait for button release.  The '$' means: jump to same instruction.
	inc stemp
	inc stemp
	inc stemp
	inc stemp
	inc stemp
	inc stemp
	inc stemp
	inc stemp
	inc stemp
	inc stemp		
	Set_Cursor(2, 13)
	mov a, stemp
	lcall Display_Accumulator
	lcall Save_Configuration_s
	clr a
	ljmp check_rest_stemp
	
down_stemp:
	jb DOWN, down_stemp10  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb DOWN, down_stemp10  ; if the 'BOOT' button is not pressed skip
	jnb DOWN, $		; Wait for button release.  The '$' means: jump to same instruction.
	dec stemp
	Set_Cursor(2, 13)
	mov a, stemp
	lcall Display_Accumulator
	lcall Save_Configuration_s
	clr a
	ljmp check_rest_stemp
	
down_stemp10:
	jb DOWNTEN, check_rest_stemp  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb DOWNTEN, check_rest_stemp  ; if the 'BOOT' button is not pressed skip
	jnb DOWNTEN, $		; Wait for button release.  The '$' means: jump to same instruction.
	dec stemp
	dec stemp
	dec stemp
	dec stemp
	dec stemp
	dec stemp
	dec stemp
	dec stemp
	dec stemp
	dec stemp
	Set_Cursor(2, 13)
	mov a, stemp
	lcall Display_Accumulator
	lcall Save_Configuration_s
	clr a
	
check_rest_stemp:
	jb MODE, check_stemp_middle  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb MODE, check_stemp_middle  ; if the 'BOOT' button is not pressed skip
	jnb MODE, $		; Wait for button release.  The '$' means: jump to same instruction.
	ljmp check_mode	

check_stemp_middle:
	ljmp check_stemp

;-------------------------------------START---------------------------------------------;
;-------------------------------------START---------------------------------------------;
;-------------------------------------START---------------------------------------------;

start_infinity:	
	
	; clear <> symbol
	Set_Cursor(1, 4)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 8)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 12)
	Send_Constant_String(#CLEAR)
	Set_Cursor(1, 16)
	Send_Constant_String(#CLEAR)	
	Set_Cursor(2, 4)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 8)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 12)
	Send_Constant_String(#CLEAR)
	Set_Cursor(2, 16)
	Send_Constant_String(#CLEAR)
	
;----------------------------------------------------------------------------------------;
;						STATE MACHINE START
;----------------------------------------------------------------------------------------;
	mov trigger, #1
	mov state, #1
	lcall Timer2_Init
    setb EA   ; Enable Global interrupts
	setb ET0
    lcall Timer0_ISR
    Wait_Milli_Seconds(#250)
    Wait_Milli_Seconds(#250)
	clr TR0
	clr ET0
loop: 

loop_c_rf:
	clr a
	mov a, destroyer
	cjne a, #1, boot
	Set_Cursor(1, 1)
	Send_Constant_String(#ERROR)
	Set_Cursor(2, 1)
	Send_Constant_String(#ERROR)
	lcall Wait7Sec		
	ljmp jump
boot:		
	clr reset_flag
	jb BOOT_BUTTON, pmw_loop ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb BOOT_BUTTON, pmw_loop  ; if the 'BOOT' button is not pressed skip
	jnb BOOT_BUTTON, $		; Wait for button release.  The '$' means: jump to same instruction.
	; A valid press of the 'BOOT' button has been detected, reset the BCD counter.
	; But first stop timer 2 and reset the milli-seconds counter, to resync everything.
	clr TR2                 ; Stop timer 2
	clr a
	mov Count1ms+0, a
	mov Count1ms+1, a
;	mov stime, a
	setb TR2                ; Start timer 2


pmw_loop:
	mov a, state
	cjne a, #2, go2powerfour
	sjmp powertwo
go2powerfour:
	ljmp powerfour
powertwo:
	setb P2.6
	Wait_Milli_Seconds(#200)
	Set_Cursor(2, 6)
	Display_BCD(rftime)
	clr P2.6
	Wait_Milli_Seconds(#200)
	Set_Cursor(2, 6)
	Display_BCD(rftime)
	Wait_Milli_Seconds(#200)
	Set_Cursor(2, 6)
	Display_BCD(rftime)
	Wait_Milli_Seconds(#200)
	Set_Cursor(2, 6)
	Display_BCD(rftime)
	Wait_Milli_Seconds(#200)
	Set_Cursor(2, 6)
	Display_BCD(rftime)
	clr a
	
	ljmp loop_a_rf 
powerfour:
	mov a, state
	cjne a, #4, go2loop_a_rf
	sjmp powerfour_implement
go2loop_a_rf:
	ljmp loop_a_rf
powerfour_implement:
	setb P2.6
	Set_Cursor(2, 6)
	Display_BCD(stime)
	Wait_Milli_Seconds(#200)
	Set_Cursor(2, 6)
	Display_BCD(stime)
	clr P2.6
	Wait_Milli_Seconds(#200)
	Set_Cursor(2, 6)
	Display_BCD(stime)
	Wait_Milli_Seconds(#200)
	Set_Cursor(2, 6)
	Display_BCD(stime)
	Wait_Milli_Seconds(#200)
	Set_Cursor(2, 6)
	Display_BCD(stime)
	Wait_Milli_Seconds(#200)
	Set_Cursor(2, 6)
	Display_BCD(stime)
	clr a
     ; Display the new value
loop_a_rf:
	jnb half_seconds_flag, checktransition_middle
	sjmp loop_b_rf
checktransition_middle:
	ljmp checktransition
loop_b_rf:
    clr half_seconds_flag ; We clear this flag in the main loop, but it is set in the ISR for timer 2
;	Set_Cursor(1, 6)     ; the place in the LCD where we want the BCD counter value
;	Display_BCD(rftime) ; This macro is also in 'LCD_4bit.inc'
;	Set_Cursor(1, 14)     ; the place in the LCD where we want the BCD counter value
;	Display_BCD(stime) ; This macro is also in 'LCD_4bit.inc'

checkstate:
	Set_Cursor(1, 1)
	Send_Constant_String(#STAGESCREEN)
	Set_Cursor(1, 7)
	Display_BCD(state)		
	lcall get_temp
	clr a
	mov a, #235
	clr c
	subb a, temp
	jnc sss
	ljmp jump
sss:
	clr a
	mov a, state		
nextstate1:
	cjne a, #1, nextstate2
	Set_Cursor(2, 1)
	Send_Constant_String(#RFSCREEN)
	Set_Cursor(2, 6)
	Display_BCD(rftime)
	Set_Cursor(2, 13)
	mov a, rftemp
	lcall Display_Accumulator
	lcall Save_Configuration_rf
	ljmp state1
nextstate2:
	cjne a, #2, nextstate3
	Set_Cursor(2, 1)
	Send_Constant_String(#RFSCREEN)
	Set_Cursor(2, 6)
	Display_BCD(rftime)
	Set_Cursor(2, 13)
	mov a, rftemp
	lcall Display_Accumulator
	lcall Save_Configuration_rf
	ljmp state2
nextstate3:
	cjne a, #3, nextstate4
	Set_Cursor(2, 1)
	Send_Constant_String(#SSCREEN)
	Set_Cursor(2, 6)
	Display_BCD(stime)
	Set_Cursor(2, 13)
	mov a, stemp
	lcall Display_Accumulator
	lcall Save_Configuration_s
	ljmp state3
nextstate4:
	cjne a, #4, nextstate5
	Set_Cursor(2, 1)
	Send_Constant_String(#SSCREEN)
	Set_Cursor(2, 6)
	Display_BCD(stime)
	Set_Cursor(2, 13)
	mov a, stemp
	lcall Display_Accumulator
	lcall Save_Configuration_s
	ljmp state4
nextstate5:
	ljmp state5	
	
;-------------------------------------STATE1---------------------------------------------;
;-------------------------------------STATE1---------------------------------------------;
;-------------------------------------STATE1---------------------------------------------;
state1:	
	setb P2.6
;    lcall Timer1_Init
	mov a, rftemp
	clr c
	subb a, temp
	jnc checktransition
	; Start state 2
	mov state, #2
	mov DPTR, #REFLOW_INIT
	lcall SendString
	clr P2.6
	ljmp checktransition
;-------------------------------------STATE2---------------------------------------------;
;-------------------------------------STATE2---------------------------------------------;
;-------------------------------------STATE2---------------------------------------------;
state2:
	mov a, rftime
	cjne a, #0x00, checkreset_middle2
	mov state, #3
checkreset_middle2:
	ljmp checktransition
	
;-------------------------------------STATE3---------------------------------------------;
;-------------------------------------STATE3---------------------------------------------;
;-------------------------------------STATE3---------------------------------------------;
state3:
	setb P2.6
	mov a, stemp
	clr c
	subb a,	temp
	jnc checktransition
	mov state, #4
	mov DPTR, #SOAK_INIT
	lcall SendString
	clr P2.6
	ljmp checktransition
;-------------------------------------STATE4---------------------------------------------;
;-------------------------------------STATE4---------------------------------------------;
;-------------------------------------STATE4---------------------------------------------;
state4:
;	mov pwm, #20
	mov	a, stime
	cjne a, #0, checktransition
	mov state, #5
	mov DPTR, #DONE
	lcall SendString
	clr P2.6
	ljmp checktransition

;-------------------------------------STATE5---------------------------------------------;
;-------------------------------------STATE5---------------------------------------------;
;-------------------------------------STATE5---------------------------------------------;
state5:	
	clr P2.6
;	mov pwm, #0
	mov a, temp
	clr c
	subb a, #60
	cpl c
	jnc backtoinitial ; if -(temp - 60) > 0 or -temp+60>0 or 60>temp go to state0
	ljmp checktransition
	
backtoinitial:
	ljmp s5_jump
	
;-------------------------------NOMORECHECKINGSTATE--------------------------------------;

checktransition:
	clr a
	mov a, rftime
	cjne a, #0, checksoak
	mov a, change_state
	cjne a, #1, checksoak
	setb ET0
	setb TR0
	;wait two seconds
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	clr TR0
	clr ET0
	mov change_state, #0
    clr a
checksoak:
	mov a, stime
	cjne a, #0, go2checkreset
	mov a, end_soak
	cjne a, #1, go2checkreset
	sjmp checksoak_cont
go2checkreset:
	ljmp checkreset
	
checksoak_cont:
	;wait 7 seconds
	setb ET0
	setb TR0
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	Wait_Milli_Seconds(#250)
	clr TR0
	clr ET0
	mov end_soak, #0
	clr a
	sjmp checkreset
	
loop_middle :
	ljmp loop
checkreset:
	jb RESET, loop_middle  ; if the 'BOOT' button is not pressed skip
	Wait_Milli_Seconds(#50)	; Debounce delay.  This macro is also in 'LCD_4bit.inc'
	jb RESET, loop_middle  ; if the 'BOOT' button is not pressed skip
	jnb RESET, $		; Wait for button release.  The '$' means: jump to same instruction.	
	ljmp jump

s5_jump:
	setb ET0
	setb TR0
	Wait_Milli_Seconds(#250)
	clr TR0
	clr ET0
	Wait_Milli_Seconds(#250)
	setb ET0
	setb TR0
	Wait_Milli_Seconds(#250)
	clr TR0
	clr ET0
	Wait_Milli_Seconds(#250)
	setb ET0
	setb TR0
	Wait_Milli_Seconds(#250)
	clr TR0
	clr ET0
	Wait_Milli_Seconds(#250)
	setb ET0
	setb TR0
	Wait_Milli_Seconds(#250)
	clr TR0
	clr ET0
	Wait_Milli_Seconds(#250)
	setb ET0
	setb TR0
	Wait_Milli_Seconds(#250)
	clr TR0
	clr ET0
	Wait_Milli_Seconds(#250)
	setb ET0
	setb TR0
	Wait_Milli_Seconds(#250)
	clr TR0
	clr ET0
	Wait_Milli_Seconds(#250)
jump:
	mov rftime, #0x10
	mov stime,  #0x10
	mov rftemp, #000
	mov stemp, #000
	mov state, #0
	mov modevar, #0
	mov trigger, #0
	mov safety, #0
	mov safety_flag, #0
	mov destroyer, #0
	mov change_state, #1
    mov end_soak, #1
	Set_Cursor(1, 1)
	Send_Constant_String(#CLEARSCREEN)
	Set_Cursor(2, 1)
	Send_Constant_String(#CLEARSCREEN)
	Set_Cursor(1, 1)
    Send_Constant_String(#TIME)
    Set_Cursor(2, 1)
    Send_Constant_String(#TEMPTEXT)
	Set_Cursor(1,6)
	Display_BCD(rftime)
	Set_Cursor(1,14)
	Display_BCD(stime)
	Set_Cursor(2, 5)
	mov a, rftemp
	lcall Display_Accumulator
	lcall Save_Configuration_rf
	Set_Cursor(2, 13)
	clr a
	mov a, stemp
	lcall Display_Accumulator
	lcall Save_Configuration_s
	clr ET2
	clr TR2
	clr EA
	clr P2.6
	setb reset_flag
	ljmp check_mode
END