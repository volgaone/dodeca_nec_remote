;******************************************************************
;
;	Programma per il microcontrollore che gestisce la scheda
;	Dodeca
;	Committente: Dodeca (Santo Pratticò)
;	Inizio stesura: 13/2/2021
;	Include file
;
;******************************************************************

; PORTA
    
    #define REMOTE  PORTA,2
    
; PORTC
    
    #define SCL	    PORTC,0
    #define SDA	    PORTC,1
    #define DAC_RST PORTC,2
    #define IN_SEL  PORTC,3
    #define UART_TX PORTC,4
    #define UART_RX PORTC,5
    
; TRISA = 0xFF
; TRISC = 0xE3