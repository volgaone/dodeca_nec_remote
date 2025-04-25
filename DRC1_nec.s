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
  
  
; System Outputs    
#define OUTPUT_LED	 PORTA,2	;Output to IR LED

; System Inputs   
#define BTTN_VOLDOWN     PORTC,1	;Input from SW2
#define BTTN_VOLUP	 PORTC,2	;Input from SW4
#define BTTN_MUTE	 PORTC,3	;Input from SW5
#define BTTN_INPDOWN     PORTC,4	;Input from SW3
#define BTTN_INPUP       PORTC,5	;Input from SW1

; Constants
SYS_PRE			 EQU 0x13       ; RC5 system address for Preamp device
MUTE_CMD		 EQU 0x0D       ; Command value for RC5 Data Mute
INPUP_CMD		 EQU 0x20       ; Command value for RC5 Data Input Up
INPDOWN_CMD		 EQU 0x21       ; Command value for RC5 Data Input Down
VOLUP_CMD		 EQU 0x10    	; Command value for RC5 Data Volume up
VOLDOWN_CMD		 EQU 0x11       ; Command value for RC5 Data Volume down
		 
; Variables
wctr2			 EQU 0x20
wctr1			 EQU 0x21
wctr0			 EQU 0x22
bsr_temp		 EQU 0x70
wreg_temp		 EQU 0x71

; Register Assignments
Delay_Count		 EQU  0x23      ; Define registers for
Delay_Count2		 EQU  0x24      ; delay routines

DataByte		 EQU  0x25      ; Define a byte to use for RC5 Data
AddrByte		 EQU  0x26      ; Define a byte to use for RC5 address
ToggByte		 EQU  0x27      ; Define a byte to determine toggle low or high
BitCount		 EQU  0x28      ; holds a bit counter as we iterate thru bits in a byte 
		 ;C			 EQU  0x00     

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
   
    
    repeat:
    call Send9msLeadingPulse
    call Delay4_5msSpace
    call NECSendZero
    call NECSendOne
    call Send562usEndBurst
    goto repeat


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
    movlw   SYS_PRE
    movwf   AddrByte            ; Load Device Address
    movlw   VOLDOWN_CMD
    movwf   DataByte            ; Load Data byte with command
    movlw   0x00
    movwf   ToggByte            ; Send Toggle=0 for button down.

    call    SendRC5
    btfss   BTTN_VOLDOWN        ; On button release, send one toggle+
    goto    voldown_action      ; Keep sending RC5 code while bttn down

    ; Send a final transmission with toggle bit = 1.
    movlw   SYS_PRE
    movwf   AddrByte            ; Load Device Address
    movlw   0x00 ;VOLDOWN_CMD         
    movwf   DataByte            ; Load Data byte with command
    movlw   0xFF
    movwf   ToggByte            ; Send Toggle=1 for button released
    call    SendRC5             ; Send button released.
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
    movlw   SYS_PRE
    movwf   AddrByte            ; Load Device Address
    movlw   VOLUP_CMD           
    movwf   DataByte            ; Load Data byte with command
    movlw   0x00                
    movwf   ToggByte            ; Send Toggle=0 for button down.
    
    call    SendRC5
    btfss   BTTN_VOLUP          ; On button release, send one toggle+
    goto    volup_action        ; Keep sending RC5 code while bttn down

    ; Send a final transmission with toggle bit = 1.
    movlw   SYS_PRE     
    movwf   AddrByte            ; Load Device Address
    movlw   0x00 ;VOLUP_CMD
    movwf   DataByte            ; Load Data byte with command
    movlw   0xFF
    movwf   ToggByte            ; Send Toggle=1 for button released
    call    SendRC5             ; Send button released.
skip_volup:

    ; MUTE BUTTON
    ;
    ; Check if button is pushed down
    btfsc   BTTN_MUTE
    goto    skip_mute           ; button is up, skip action code
    call    DebounceDelay       ; Short debounce delay
    btfsc   BTTN_MUTE           ; check if button is still down 
    goto    skip_mute           ; bttn is up, skip action code (false indicator)

    ; Detected the button as pressed.  Send keydown code.

    ; Button's Action Code
    ;
    ; Repeatedly send RC5 transmission with toggle bit = 0.
mute_action:
    movlw   SYS_PRE
    movwf   AddrByte            ; Load Device Address
    movlw   MUTE_CMD           
    movwf   DataByte            ; Load Data byte with command
    movlw   0x00                
    movwf   ToggByte            ; Send Toggle=0 for button down.
    
    call    SendRC5
    btfss   BTTN_MUTE          ; On button release, send one toggle+
    goto    mute_action        ; Keep sending RC5 code while bttn down

    ; Send a final transmission with toggle bit = 1.
    movlw   SYS_PRE     
    movwf   AddrByte            ; Load Device Address
    movlw   0x00 ;MUTE_CMD
    movwf   DataByte            ; Load Data byte with command
    movlw   0xFF
    movwf   ToggByte            ; Send Toggle=1 for button released
    call    SendRC5             ; Send button released.
skip_mute:


    ; INPUT DOWN BUTTON
    ;
    ; Check if button is pushed down
    btfsc   BTTN_INPDOWN
    goto    skip_inpdown        ; button is up, skip action code
    call    DebounceDelay       ; Short debounce delay
    btfsc   BTTN_INPDOWN        ; check if button is still down 
    goto    skip_inpdown        ; bttn is up, skip action code (false indicator)

    ; Detected the button as pressed.  Send keydown code.

    ; Button's Action Code
    ;
    ; Repeatedly send RC5 transmission with toggle bit = 0.
inpdown_action:
    movlw   SYS_PRE
    movwf   AddrByte            ; Load Device Address
    movlw   INPDOWN_CMD           
    movwf   DataByte            ; Load Data byte with command
    movlw   0x00                
    movwf   ToggByte            ; Send Toggle=0 for button down.
    
    call    SendRC5
    btfss   BTTN_INPDOWN          ; On button release, send one toggle+
    goto    inpdown_action        ; Keep sending RC5 code while bttn down

    ; Send a final transmission with toggle bit = 1.
    movlw   SYS_PRE     
    movwf   AddrByte            ; Load Device Address
    movlw   0x00 ;INPDOWN_CMD
    movwf   DataByte            ; Load Data byte with command
    movlw   0xFF
    movwf   ToggByte            ; Send Toggle=1 for button released
    call    SendRC5             ; Send button released.
skip_inpdown:


    ; INPUT UP BUTTON
    ;
    ; Check if button is pushed down
    btfsc   BTTN_INPUP
    goto    skip_inpup          ; button is up, skip action code
    call    DebounceDelay       ; Short debounce delay
    btfsc   BTTN_INPUP          ; check if button is still down 
    goto    skip_inpup          ; bttn is up, skip action code (false indicator)

    ; Detected the button as pressed.  Send keydown code.

    ; Button's Action Code
    ;
    ; Repeatedly send RC5 transmission with toggle bit = 0.
inpup_action:
    movlw   SYS_PRE
    movwf   AddrByte            ; Load Device Address
    movlw   INPUP_CMD           
    movwf   DataByte            ; Load Data byte with command
    movlw   0x00                
    movwf   ToggByte            ; Send Toggle=0 for button down.
    
    call    SendRC5
    btfss   BTTN_INPUP          ; On button release, send one toggle+
    goto    inpup_action        ; Keep sending RC5 code while bttn down

    ; Send a final transmission with toggle bit = 1.
    movlw   SYS_PRE     
    movwf   AddrByte            ; Load Device Address
    movlw   0x00 ;INPUP_CMD
    movwf   DataByte            ; Load Data byte with command
    movlw   0xFF
    movwf   ToggByte            ; Send Toggle=1 for button released
    call    SendRC5             ; Send button released.
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

NECSendZero:
movlw   23               ; -1    
movwf   Delay_Count2        ; -1    num pulses counter
call NECCarrierLoop
bcf     OUTPUT_LED          ; -1    (BEGIN OFF TIME)
movlw   176                ; +1 us
movwf   Delay_Count         ; +1 
decfsz  Delay_Count, F      ; Decrement F, skip if result = 0
goto    $-1                 ; Go back 1, keep decrementing until     
return
    
    
NECSendOne:
movlw   23               ; -1    
movwf   Delay_Count2        ; -1    num pulses counter
call NECCarrierLoop
call delay_1ms
bcf     OUTPUT_LED          ; -1    (BEGIN OFF TIME)
movlw   218                ; +1 us
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

    rlf     AddrByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one
    

    return                      ; Return from SendRC5 routine    
    
;******************************************************************
;  SUB-ROUTINES
;******************************************************************

;------------------------------------------------------------------
;  SendRC5
;
;       Before calling, preload registers DataByte and AddrByte with
;   appropriate values to be sent.
; 
;   DataByte = 6 bits of data to send (upper 2 bits ignored)
;   AddrByte = 5 bits of addr to identify target (upper 3 bits ignored)
;------------------------------------------------------------------
SendRC5:

    ; Pre-shift Addr Byte
    rlf     AddrByte, F     ; Must be rotated left 3 bits
    rlf     AddrByte, F     ;
    rlf     AddrByte, F     ; MSB now is MSB of 5 bit #
    ; Pre-shift Data Byte
    rlf     DataByte, F     ; Must be rotated left 2 bits
    rlf     DataByte, F     ; MSB now is MSB of 6 bit #

    ; SEND PREAMBLE
    call    SendOne         ; S1        Start 1
    call    SendOne         ; S2        Start 2

    ; SEND TOGGLE
    btfss   ToggByte, 0     ; if toggle is one, skip instr
    call    SendZero        ; Send a 0, toggle byte is zero
    btfsc   ToggByte, 0     ; 
    call    SendOne         ; toggle byte is set, need to send a 1


    ; SEND DATA IN SPEED EFFICIENT MANNER

    ; SEND ADDRESS
    ; Begin shifting out address
    ; bit 4
    rlf     AddrByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one
    ; bit 3
    rlf     AddrByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one
    ; bit 2
    rlf     AddrByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one
    ; bit 1
    rlf     AddrByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one
    ; bit 0
    rlf     AddrByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one

    
    ; SEND DATA
    ; Shift Out DataByte
    ; bit 5
    rlf     DataByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one
    ; bit 4
    rlf     DataByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one
    ; bit 3
    rlf     DataByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one
    ; bit 2
    rlf     DataByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one
    ; bit 1
    rlf     DataByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one
    ; bit 0
    rlf     DataByte, F     ; Shift out MSB.. C = MSB
    btfss   CARRY           ; if bit is 1, skip next instr.
    call    SendZero        ; bit is 0, send a zero
    btfsc   CARRY
    call    SendOne         ; bit is 1, send a one  
    
    ; 25 ms passed

    ; Delay remaining time so that repetitive calls to SendRC5
    ; occur at 114ms intervals as per RC5 spec.

    ; 114-25 = 89ms
    bcf     OUTPUT_LED          ; Set output low for off time

    movlw   89
    movwf   Delay_Count2        ; outer loop delay counter

    call    delay_1ms           ; Delay 1ms ea. time through loop
    decfsz  Delay_Count2, F     ; Go through loop
    goto    $-2                 ; if count not 0, keep looping


    return                      ; Return from SendRC5 routine



;------------------------------------------------------------------
;  SendOne
;
;       A "1" in Manchester goes from Low-High over the bit period
;   Timing based off 4MHz internal clock with 1MHz Instruction
;   cycle. During the pulsing period, the carrier frequency should
;   be 36kHz, and the duty cycle of the carrier should be about 1/4.
;------------------------------------------------------------------
SendOne:

    ; LOW HALF (889us = 889 instr cycles)
    bcf     OUTPUT_LED          ; Turn off LED
        ; 1 --> 888us           ; (-1) instr cycle from total needed
    ;
    movlw   0xFF                ; (-1) Move 0xFF (255) into w
    movwf   Delay_Count         ; (-1) Move w -> Delay_Count
    decfsz  Delay_Count, F      ; (-1) Decrement F, skip if result = 0
    goto    $-1                 ; (-2) Go back 1, keep decrementing until 0
        ; Loop Eq. = 3*N-1 cycles
        ; 3*254 = 764us completed in loop, + 3 cycles beforehand..
        ; 767us completed --> 122us to go.
    ;
    movlw   39               ; -1 (Load to finish time accurately)
    movwf   Delay_Count         ; -1
    decfsz  Delay_Count, F      ; -1
    goto    $-1                 ; -2
        ; NOTE: there are two cycles following this 
        ;       before pulsing will start.. so take 2 cycles off desired
        ; 1 + 1 + 3*N-1 = 122 - 2 --> N=39.66
        ; Choose N = 39, gives 116 cycles in loop, +2 setup, +2 lagging
        ; = 116+2+2 = 120   -->   need 2 nops, or 1 goto $+1
    goto    $+1                 ; -2

    ;
    ; HIGH HALF (889us)
    ; Toggle 7us on, 21us off for 35.7kHz (~36kHz) for the duration
    ; Pulsing 7 on, 21 off yields a 1/4 time duty cycle for carrier.
    ; Has 32 pulses of these periods 32*28us = 896us (~889us)
    ;
    ; These two clock cycles contribute to LOW TIME
    movlw   32               ; -1    (2 addit'l low cycles on low time)
    movwf   Delay_Count2        ; -1    num pulses counter

CarrierLoopOne:
    bsf     OUTPUT_LED          ; -1    (BEGIN ON TIME)
    goto    $+1                 ; -2us
    goto    $+1                 ; -2us
    goto    $+1                 ; -2us  delayed 7us
    bcf     OUTPUT_LED          ; -1    (BEGIN OFF TIME)
    movlw   5                ; -1 (Load to finish time accurately)
    movwf   Delay_Count         ; -1
    decfsz  Delay_Count, F      ; -1
    goto    $-1                 ; -2
        ; 1 + 1 + 1 + 3*N-1 = 21-3  --> x = 5.33
        ; Choose N=5, 1+1+1 + LOOP=14 =17 --> need 1 nop
    nop
    decfsz  Delay_Count2, F     ; -1  3us tacked on each pulse xcept last one
    goto    CarrierLoopOne      ; -2  TAKE OFF OF ABOVE CALC

    ; DONE Sending a one
    return                      ; -2 return from subroutine
    


;------------------------------------------------------------------
;  SendZero
;
;       A "0" in Manchester goes from High-Low over the bit period.
;   The high period is a series of pulses of duty cycle 1/4 at a
;   frequency of 36kHz.  This implementation yields 35.71kHz.
;------------------------------------------------------------------
SendZero:

    ; HIGH HALF (889us)
    ; Toggle 7us on, 21us off for 35.7kHz (~36kHz) for the duration
    ; Pulsing 7 on, 21 off yields a 1/4 time duty cycle for carrier.
    ; Has 32 pulses of these periods 32*28us = 896us (~889us)
    ;
    ; These two clock cycles contribute to LOW TIME
    movlw   32               ; -1    (2 addit'l low cycles on low time)
    movwf   Delay_Count2        ; -1    num pulses counter

CarrierLoopZero:
    bsf     OUTPUT_LED          ; -1    (BEGIN ON TIME)
    goto    $+1                 ; -2us
    goto    $+1                 ; -2us
    goto    $+1                 ; -2us  delayed 7us
    bcf     OUTPUT_LED          ; -1    (BEGIN OFF TIME)
    movlw   5                ; -1 (Load to finish time accurately)
    movwf   Delay_Count         ; -1
    decfsz  Delay_Count, F      ; -1
    goto    $-1                 ; -2
        ; 1 + 1 + 1 + 3*N-1 = 21-3  --> x = 5.33
        ; Choose N=5, 1+1+1 + LOOP=14 =17 --> need 1 nop
    nop
    decfsz  Delay_Count2, F     ; -1  3us tacked on each pulse xcept last one
    goto    CarrierLoopZero     ; -2  TAKE OFF OF ABOVE CALC

    ; Last pulse needs its off time 
    ; that it misses from goto CarrierLoop
    goto    $+1                 ; -2

    ; LOW HALF (889us = 889 instr cycles)
    bcf     OUTPUT_LED          ; Turn off LED
        ; 1 --> 888us           ; (-1) instr cycle from total needed
    ;
    movlw   0xFF                ; (-1) Move 0xFF (255) into w
    movwf   Delay_Count         ; (-1) Move w -> Delay_Count
    decfsz  Delay_Count, F      ; (-1) Decrement F, skip if result = 0
    goto    $-1                 ; (-2) Go back 1, keep decrementing until 0
        ; Loop Eq. = 3*N-1 cycles
        ; 3*254 = 764us completed in loop, + 3 cycles beforehand..
        ; 767us completed --> 122us to go.
    ;
    movlw   39               ; -1 (Load to finish time accurately)
    movwf   Delay_Count         ; -1
    decfsz  Delay_Count, F      ; -1
    goto    $-1                 ; -2
        ; NOTE: there are two cycles following this  (return)
        ;       before next bit may be sent.
        ; 1 + 1 + 3*N-1 = 122 - 2 --> N=39.66
        ; Choose N = 39, gives 116 cycles in loop, +2 setup, +2 lagging
        ; = 116+2+2 = 120   -->   return finishes the last 2 instr cycl.

    return                      ; -2


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

;
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

