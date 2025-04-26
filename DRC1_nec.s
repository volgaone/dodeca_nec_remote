;******************************************************************
;   AUTHOR:     Santo Prattico'
;   COMPANY:    Dodeca s.r.l.s
;   DEVICE:     PIC16F15225 (changed from PIC16LF1554)
;   CREATED:    23/04/2021
;   UPDATED:    25/04/2025
;
;   DESCRIP:    Code for Remote control DRC1 with NEC protocol.        
;
;******************************************************************

    PROCESSOR 16F15225

    #include <xc.inc>

; Config Bits
; CONFIG1
  CONFIG  RSTOSC = 0x00  	       	; Oscillator Selection Bits (internal oscillator)
  CONFIG  WDTE = OFF            	; Watchdog Timer Enable (WDT disabled. SWDTEN bit is ignored.)
;  CONFIG  PWRTE = ON            	; Power-up Timer Enable (PWRT enabled)
;  CONFIG  MCLRE = ON            	; MCLR Pin Function Select (MCLR/VPP pin function is MCLR)
  CONFIG  CP = OFF              	; Flash Program Memory Code Protection (Program memory code protection is disabled)
  ;CONFIG  BOREN = OFF           	; Brown-out Reset Enable (Brown-out Reset disabled. SBOREN bit is ignored.)
  CONFIG  CLKOUTEN = OFF        	; Clock Out Enable (CLKOUT function is disabled. I/O or oscillator function on the CLKOUT pin)

; CONFIG2
  CONFIG  PWRTS = 0x10			; Power-up Timer Enable (PWRT enabled) at 64msec
  CONFIG  MCLRE = 0x01             	; MCLR Pin Function Select (MCLR/VPP pin function is MCLR)
;  CONFIG  WRT = OFF             	; Flash Memory Self-Write Protection (Write protection off)
  CONFIG  STVREN = ON           	; Stack Overflow/Underflow Reset Enable (Stack Overflow or Underflow will cause a Reset)
  CONFIG  BORV = LO             	; Brown-out Reset Voltage Selection (Brown-out Reset Voltage (Vbor), 1.9V trip point selected.)
;  CONFIG  LPBOR = OFF           	; Low-Power Brown Out Reset (Low-Power BOR is disabled)
  CONFIG  LVP = 0x01                 	; Low-Voltage Programming Enable (Low-voltage programming enabled)
  CONFIG  BOREN = OFF           	; Brown-out Reset Enable (Brown-out Reset disabled. SBOREN bit is ignored.)
; 
;  CONFIG4  
  CONFIG  WRTSAF = 0x01             	; Flash Memory Self-Write Protection (Write protection off)
  CONFIG  WRTC = 0x01             	; Flash Memory Self-Write Protection (Write protection off)
  CONFIG  WRTB = 0x01             	; Flash Memory Self-Write Protection (Write protection off)
  CONFIG  WRTAPP = 0x01             	; Flash Memory Self-Write Protection (Write protection off)
  
  
 ZERO_BIT EQU 2				; in STATUS register
  
; System Outputs    
#define OUTPUT_LED	 PORTA,2	;Output to IR LED

; System Inputs   
#define BTTN_VOLDOWN     PORTC,1	;Input from SW2
#define BTTN_VOLUP	 PORTC,2	;Input from SW4
#define BTTN_MUTE	 PORTC,3	;Input from SW5
#define BTTN_INPDOWN     PORTC,4	;Input from SW3
#define BTTN_INPUP       PORTC,5	;Input from SW1

; Constants
SYS_ADDRESS		 EQU 0x89       ; device address
MUTE_CMD		 EQU 0x1B       ; short button press
POWER_CMD		 EQU 0xBF
INP1_CMD		 EQU 0xE6       ; Command value for RC5 Data Input Up
INP2_CMD		 EQU 0xE7       ; Command value for RC5 Data Input Down
INP3_CMD		 EQU 0xEF
VOLUP_CMD		 EQU 0xE9    	; Command value for RC5 Data Volume up
VOLDOWN_CMD		 EQU 0xE5       ; Command value for RC5 Data Volume down
		 
; Variables
wctr2			 EQU 0x20
wctr1			 EQU 0x21
wctr0			 EQU 0x22
bsr_temp		 EQU 0x70
wreg_temp		 EQU 0x71

; Register Assignments
Delay_Count		 EQU  0x23      ; Define registers for
Delay_Count2		 EQU  0x24      ; delay routines
Delay_Count3		 EQU  0x2C      ; delay routines

DataByte		 EQU  0x25      ; Define a byte to use for RC5 Data
AddrByte		 EQU  0x26      ; Define a byte to use for RC5 address
ToggByte		 EQU  0x27      ; Define a byte to determine toggle low or high
BitCount		 EQU  0x28      ; holds a bit counter as we iterate thru bits in a byte 
NEC_byte		 EQU  0x29	; holds the byte to be sent over NEC protocol
Input			 EQU  0x2A      ; current input; can be 1, 2 or 3 (corresponding to INPx_CMD)
PressLengthCount	 EQU  0x2B      ; counts how long we hold the mute button for, in increments of 10ms
;------------------------------------------------------------------
;  PROGRAMME CODE
;------------------------------------------------------------------
PSECT code, abs
  
ResVctr:
    goto	start			;
     
    ORG 0x0004		;

start:
    clrw                       		;Processor Reset vector
    BANKSEL (OSCFRQ)
    movlw   0x02			;
    movwf   OSCFRQ			;4MHz from internal oscillator
    BANKSEL (ANSELA)
    clrf    ANSELA			;All port A pins are digital
    clrf    ANSELC			;All port C pins are digital

;Set up the ports
    
    BANKSEL(TRISA)			;
    movlw   0xFB			; Port A 2 is the LED output
    movwf   TRISA			;
    movlw   0xFE			; Port C all inputs, except RC0 which is output - we'll use it for debug
    movwf   TRISC			; 
    BANKSEL(PORTC)
    bcf PORTC, 0   
    BANKSEL (WPUC)			;
    movlw   0x3F			;
    movwf   WPUC			; Enable weak pull-ups on RC0-RC5 (buttons)

    BANKSEL(PORTA)
    bcf     OUTPUT_LED                  ; Init LED output off
   
    BANKSEL(IOCCN)
    movlw 0x3E
    movwf IOCCN				;Interrupt-on-change enabled on the IOCC pins for a negative-going edge
    BANKSEL(PIE0)
    bsf PIE0,4				;Interrupt-on-Change Enable
    BANKSEL(INTCON)
    bsf INTCON, 6			;PEIE Peripheral Interrupt Enable
   
    BANKSEL(Input)
    movlw INP1_CMD
    movwf Input                         ;set a default value for the Input

Main_Loop:
    
    ; VOLUME_DOWN BUTTON
    ;
    ; Check if button is pushed down
    btfsc   BTTN_VOLDOWN        
    goto    skip_voldown        ; button is up, skip action code
    call    DebounceDelay       ; Short debounce delay
    btfsc   BTTN_VOLDOWN        ; check if button is up (false indicator)
    goto    skip_voldown        ; bttn is up, skip action code
    ; Detected the button as pressed.  Send keydown code.
    ; Button's Action Code
    ;
    ; Repeatedly send RC5 transmission with toggle bit = 0.
voldown_action:
    movlw   SYS_ADDRESS
    movwf   AddrByte            ; Load Device Address
    movlw   VOLDOWN_CMD
    movwf   DataByte            ; Load Data byte with command

    call    SendNECCommand
    btfss   BTTN_VOLDOWN        ; On button release, send one toggle+
    goto    voldown_action      ; Keep sending RC5 code while bttn down
    
skip_voldown:


    ; VOLUME UP BUTTON
    ;
    ; Check if button is pushed down
    btfsc   BTTN_VOLUP
    goto    skip_volup          ; button is up, skip action code
    call    DebounceDelay       ; Short debounce delay
    btfsc   BTTN_VOLUP          ; check if button is still down 
    goto    skip_volup          ; bttn is up, skip action code (false indicator)

    ; Detected the button as pressed.  Send keydown code.

    ; Button's Action Code
    ;
    ; Repeatedly send RC5 transmission with toggle bit = 0.
volup_action:
    movlw   SYS_ADDRESS
    movwf   AddrByte            ; Load Device Address
    movlw   VOLUP_CMD           
    movwf   DataByte            ; Load Data byte with command
   
    call    SendNECCommand
    btfss   BTTN_VOLUP          ; On button release, send one toggle+
    goto    volup_action        ; Keep sending RC5 code while bttn down

skip_volup:

    ; MUTE BUTTON
    ;
    ; Check if button is pushed down
    
    btfsc   BTTN_MUTE
    goto    skip_mute_and_power           ; button is up, skip action code
    
    movlw   0
    movwf   PressLengthCount
measure_mute_hold:    
    call    delay_50ms           
    incf    PressLengthCount
    movlw   20 ;one second = 20*50ms
    subwf   PressLengthCount, W
    btfsc   CARRY   ;carry is 0 if the result of subtraction is negative - PressLengthCount is smaller than W - so have to execute MUTE   
    goto    power_action
    btfss   BTTN_MUTE                   ; check if button is still down 
    goto    measure_mute_hold           ; bttn is still down, go back to measuring the duration of how long it's been pressed for
    ; button is up - let's see how long it's been pressed for
    ; the threshold is 1 second for POWER ON/OFF vs mute, which is short press. 1 second = 100 10ms increments
    
    
mute_action:
    movlw   SYS_ADDRESS
    movwf   AddrByte            ; Load Device Address
    movlw   MUTE_CMD           
    movwf   DataByte            ; Load Data byte with command
    
    call    SendNECCommand
    call    delay_500ms
    goto    skip_mute_and_power
    
power_action:
    movlw   SYS_ADDRESS
    movwf   AddrByte            ; Load Device Address
    movlw   POWER_CMD           
    movwf   DataByte            ; Load Data byte with command
    
    call    SendNECCommand
    call    delay_500ms
skip_mute_and_power:


    ; INPUT DOWN BUTTON
    ;
    ; Check if button is pushed down
    btfsc   BTTN_INPDOWN
    goto    skip_inpdown        ; button is up, skip action code
    call    DebounceDelay       ; Short debounce delay
    btfsc   BTTN_INPDOWN        ; check if button is still down 
    goto    skip_inpdown        ; bttn is up, skip action code (false indicator)

inpdown_action:
    movlw   SYS_ADDRESS
    movwf   AddrByte            ; Load Device Address
    call    decrement_input
    movf    Input,W          
    movwf   DataByte            ; Load Data byte with the new, changed input
    
    call    SendNECCommand
    call    delay_500ms
    btfss   BTTN_INPDOWN          ; On button release, send one toggle+
    goto    inpdown_action        ; Keep sending RC5 code while bttn down

skip_inpdown:


    ; INPUT UP BUTTON
    ;
    ; Check if button is pushed down
    btfsc   BTTN_INPUP
    goto    skip_inpup          ; button is up, skip action code
    call    DebounceDelay       ; Short debounce delay
    btfsc   BTTN_INPUP          ; check if button is still down 
    goto    skip_inpup          ; bttn is up, skip action code (false indicator)

inpup_action:
    movlw   SYS_ADDRESS
    movwf   AddrByte            ; Load Device Address
    call    increment_input
    movf    Input, W          
    movwf   DataByte            ; Load Data byte with command
    
    call    SendNECCommand
    call    delay_500ms
    btfss   BTTN_INPUP         ; Send button released.
skip_inpup:
    BANKSEL(IOCCF)
    movlw 0x0
    movwf IOCCF			 ; clear any pending IOC flags for PORTC, so that they don't wake us up from sleep
    
    BANKSEL(PORTC)
    bsf PORTC, 0		 ;set RC0 just as we go into sleep so that we can observe it 
    SLEEP
    nop
    bcf PORTC, 0
    goto Main_Loop               ; forever run in loop

;============================================
; sub-routines
    
NECSendZero:
    movlw   23                  ;    
    movwf   Delay_Count2        ; pass the argument to NECCarrierLoop
    call NECCarrierLoop
    bcf     OUTPUT_LED          ; -1    (BEGIN OFF TIME)
    movlw   176                 ; +1 us
    movwf   Delay_Count         ; +1 
    decfsz  Delay_Count, F      ; Decrement F, skip if result = 0
    goto    $-1                 ; Go back 1, keep decrementing until     
    return
    
    
NECSendOne:
    movlw   23                  ;    
    movwf   Delay_Count2        ; pass the argument to NECCarrierLoop
    call NECCarrierLoop
    call delay_1ms
    bcf     OUTPUT_LED          ; -1    (BEGIN OFF TIME)
    movlw   218                 ; +1 us
    movwf   Delay_Count         ; +1 
    decfsz  Delay_Count, F      ; Decrement F, skip if result = 0
    goto    $-1                 ; Go back 1, keep decrementing until
    return    

Send9msLeadingPulse:
    movlw   255               ; -1    
    movwf   Delay_Count2        ; -1    num pulses counter
    call NECCarrierLoop
    movlw   93               ; -1    
    movwf   Delay_Count2        ; -1    num pulses counter    
    call NECCarrierLoop
    return
    
Send562usEndBurst:
    movlw   23               ; -1    
    movwf   Delay_Count2        ; -1    num pulses counter
    call NECCarrierLoop
    return
    
NECCarrierLoop:
    bsf     OUTPUT_LED          ; -1    (BEGIN ON TIME)
    nop				; -1us
    goto    $+1                 ; -2us
    goto    $+1                 ; -2us  delayed 6 us
    bcf     OUTPUT_LED          ; -1    (BEGIN OFF TIME)
    movlw   5                ; -1 (Load to finish time accurately)
    movwf   Delay_Count         ; -1
    decfsz  Delay_Count, F      ; -1
    goto    $-1                 ; -2
        ; 1 + 1 + 1 + 3*N-1 = 21-3  --> x = 5.33
    decfsz  Delay_Count2, F     ; -1  3us tacked on each pulse xcept last one
    goto    NECCarrierLoop      ; -2  TAKE OFF OF ABOVE CALC
    return
    
    
Delay4_5msSpace:
    movlw   4                ; -1 (Load to finish time accurately)
    movwf   Delay_Count2         ; -1
    call  delay_1ms
    decfsz  Delay_Count2, F      ; -1
    goto $-2
    movlw   150                ; +1 us
    movwf   Delay_Count         ; +1 
    decfsz  Delay_Count, F      ; Decrement F, skip if result = 0
    goto    $-1                 ; Go back 1, keep decrementing until 
    return


SendNECbyte:
    movlw 8
    movwf BitCount
NEC_bit:
    rrf     NEC_byte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    NECSendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    NECSendOne         ; bit is 1, send a one
    decfsz  BitCount, F
    goto NEC_bit
    return                      ; Return from SendRC5 routine    

; takes arguments in AddrByte, DataByte
SendNECCommand:
    call Send9msLeadingPulse
    call Delay4_5msSpace
    ;  send 2 address bytes, second one being logical inverse of the first
    movf AddrByte,W
    movwf NEC_byte
    call SendNECbyte
    comf AddrByte, W //logical NOT on AddrByte, transfer to working register
    movwf NEC_byte
    call SendNECbyte
    ;  send 2 data bytes, second one being logical inverse of the first
    movf DataByte,W
    movwf NEC_byte
    call SendNECbyte
    comf DataByte, W
    movwf NEC_byte
    call SendNECbyte
    call Send562usEndBurst
    call delay_10ms		; insert 10ms delay between the commands
    return
    
decrement_input:    
    movf Input,W
    
    xorlw INP1_CMD
    btfss STATUS,ZERO_BIT
    goto check_input2_dec
    movlw INP3_CMD
    goto end_decrement
check_input2_dec:
    xorlw INP2_CMD
    btfss STATUS,ZERO_BIT
    goto check_input3_dec
    movlw INP1_CMD
    goto end_decrement
check_input3_dec:
    xorlw INP3_CMD
    btfsc STATUS,ZERO_BIT
    movlw INP2_CMD
    
end_decrement:
    movwf Input
    return
    
    
increment_input:
    movf Input,W
    
    xorlw INP1_CMD
    btfss STATUS,ZERO_BIT
    goto check_input2_inc
    movlw INP2_CMD
    goto end_inc
check_input2_inc:
    xorlw INP2_CMD
    btfss STATUS,ZERO_BIT
    goto check_input3_inc
    movlw INP3_CMD
    goto end_inc
check_input3_inc:
    xorlw INP3_CMD
    btfsc STATUS,ZERO_BIT
    movlw INP1_CMD
    
end_inc:
    movwf Input
    return


;--------------------------------------------------------------------
;
;   Salvataggio e recupero di registri di uso comune quando
;   si chiama una funzione o una macro
;
;--------------------------------------------------------------------

save_registers:
    movwf   wreg_temp			;
    movf    BSR,W			;
    movwf   bsr_temp			;
    return				;

restore_registers:
    movf    bsr_temp,W			;
    movwf   BSR				;
    movf    wreg_temp,W			;
    return				;

;
;*********************************************************************************    

;------------------------------------------------------------------
;  delay_1ms
;
;       Precise delay.  Actually delays 999us, but this allows for
;   any loops calling it to incur 1us of overhead.
;------------------------------------------------------------------
delay_1ms:
    movlw   0xFF                ; +1 us
    movwf   Delay_Count         ; +1 
    decfsz  Delay_Count, F      ; Decrement F, skip if result = 0
    goto    $-1                 ; Go back 1, keep decrementing until 0
                                ; Loop = 3*N-1 us = 3*255-1
                                ; +764
    ; Need to delay 236us more.  2 of which will be used in return
    movlw   76               ; +1
    movwf   Delay_Count         ; +1 234 remain, but only acct for 232
    decfsz  Delay_Count, F      ; 3*N-1 = 232 --> N = 77.66
    goto    $-1                 ; Choose N=77


    return                      ; +2 Return program flow

delay_10ms:
    movlw 10
    movwf Delay_Count2
    call delay_1ms
    decfsz Delay_Count2,F
    goto $-2     
    return
    
delay_50ms:
    movlw 50
    movwf Delay_Count2
    call delay_1ms
    decfsz Delay_Count2,F
    goto $-2     
    return
    
delay_500ms:
    movlw 50
    movwf Delay_Count3
    call delay_10ms
    decfsz Delay_Count3,F
    goto $-2     
    return    

;------------------------------------------------------------------
;  DebounceDelay
;
;       Small and simple delay to provide time for bouncing from
;   a button to settle.
;------------------------------------------------------------------
DebounceDelay:
    call    save_registers		;
    BANKSEL(Delay_Count)
    movlw   0x03                ; Move decimal 3 into w (count, N)
    movwf   Delay_Count         ; Move w -> Delay_Count
    decfsz  Delay_Count, F      ; Decrement Delay_Count (F means result goes back into same variable), skip if result = 0
    goto    $-1                 ; Go back 1, keep decrementing until 0
                                ; Loop delay = 3*N-1
    call    restore_registers	;
    return                      ; Return program flow
                                ; TOTAL DELAY ~12us
;
;
;
;``````````````````````````````````````````````````````````````````
    END

