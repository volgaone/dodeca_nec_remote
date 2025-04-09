/*
 * File:   main.c
 * Author: Dmitry
 *
 * Created on April 9, 2025, 7:50 AM
 */


#include <xc.h>

__CONFIG(FOSC_INTOSCIO & WDTE_OFF & PWRTE_OFF & MCLRE_OFF & CP_OFF & IOSCFS_8MHZ & BOREN_NSLEEP);

#define _XTAL_FREQ 8000000			// required for delay Routines. 

#define IR_LED 	PORTAbits_t.RA2		//IR LED is conntected to this port
	
#define KEY_1	PORTCbits.RC5		//Switces pin definition 
#define KEY_2	PORTCbits.RC1
#define KEY_3	PORTCbits.RC4
#define KEY_4	PORTCbits.RC2
#define KEY_5	PORTCbits.RC3

#define ADDRESS 0x89				//NEC protocol address for the Data Frame (MSB first)
									//if you are planning to use this with http://www.circuitvalley.com/2012/09/infrared-ir-nec-microcontroller-pic-avr.html IR realy borad then this field can be any thing because the Receiver does't look for this. 
									//if you are planning to use the remote with some other target then please put the address here is MSB first order
									//you can have different address for different keys. please modify the GPIO interrupt on change routine. 

#define DATA_KEY_1	0xF8			//Data For KEY_1 (MSB first)
#define DATA_KEY_2	0x50			//Data For KEY_2 (MSB first)
#define DATA_KEY_3	0xD8			//Data For KEY_3 (MSB first)
#define DATA_KEY_4	0xB0			//Data For KEY_4 (MSB first)
#define DATA_KEY_5	0x30			//Data For KEY_5 (MSB first)

void sendFrame(unsigned char , unsigned char );		//sendFrame(address,data) sends the whole IR frame
void sendByte(unsigned char );						//sendByte(byte) this function is used by the sendFrame to send 4 indivisual btyes
													//SHOULD NOT BE CALL FROM ANY WERE ELSE except the sendFrame Function.
void sendRepeat();									//Sends the Repeate Code after Message Frame.				

void swInt(void)		
{	
   
	if (IOCAN != 0)					// check the interrupt on change flag
	{	
		__delay_ms(2);						//check for key debounce			
	
		if(!KEY_1)							//check if the Key_1 is pressed (gpio is low)
		{
			sendFrame(ADDRESS,DATA_KEY_1);	//send the frame
	 		while(!KEY_1)					//if the key is still pressed 
			{
			sendRepeat();					//send Repeat codes. 
			}	
		}else if(!KEY_2)					//check if the Key_2 is pressed
		{
			sendFrame(ADDRESS,DATA_KEY_2);	//send the frame
			while(!KEY_2)					//if the key is still pressed 
			{
			sendRepeat();					//send Repeat codes.
			}
		}else if(!KEY_3)					//check if the Key_3 is pressed
		{
			sendFrame(ADDRESS,DATA_KEY_3);	//send the frame
			while(!KEY_3)					//if the key is still pressed 	
			{
			sendRepeat();					//send Repeat codes.
			}
		}else if(!KEY_4)					//check if the Key_4 is pressed
		{
			sendFrame(ADDRESS,DATA_KEY_4);	//send the frame
			while(!KEY_4)					//if the key is still pressed 	
			{
			sendRepeat();					//send Repeat codes.	
			}
		}else if(!KEY_5)					//check if the Key_5 is pressed
		{
			sendFrame(ADDRESS,DATA_KEY_5);	//send the frame
			while(!KEY_5)					//if the key is still pressed 
			{
			sendRepeat();					//send Repeat codes.
			}
		}
		KEY_1;								//this is requited to end the mismatch condition 
	//TODO: clear the edge
    }
}


void main()
{
    ANSELC		= 0x00;		// all pin are Digital
    TRISC		= 0x3B; 	// Only PORTC pins are inputs
	//OPTION_REG 	= 0x5F;		//pull-ups are enabled	
							//timer0 clock source is internal
							//timer0 pre-scaler is 1:1 (disabled "assigned to WDT")

	WPUC 	= 0x3B;			//week pull-ups are enabled for all switches.
	PORTC 	= 0x3B;			//put hight to the latch value of each GPIO
	T2CON 	= 0x04;			// pre-scaler 1:1 (Timer2 is required BY the ECCP module to generate 38KHz Frequency) 
							// Timer2 is on	
							// post-scaler 1:1
			
	CCPR1L			= 0x1A;	//CCP module comparison resistor to set 50% duty for 38KHz signal
	PR2 			= 51;	//CCP module period register to generate 38KHz signal
	CCP1CON 		= 0x20;	//set Two LSB of PWM Duty 8MSB are in CCPR1L the module will turned on later 
	IOCCN 			= 0x3B; //interrupt on negative-going change is assigned to all the port pin.
	INTCONbits.GIE = 1;	// external interrupt on GPIO3 pin(4) is enabled
	//INTCONbits.GPIF = 0;	// clear the external interrupt flag	
	INTCONbits.PEIE = 1;    // peripheral interrupt enable
 
	while(1)
	{
		SLEEP();			//put the MCU into Sleep mode forever , will wake-up when ever any key press is detected. 
							// the measured current consumption in this mode is ~ 29na ( much lower then the datasheet claim 50na)
	}
}

void sendFrame(unsigned char address, unsigned char command) // this routine send the whole frame including 9ms leading pulse 4.5ms space address ~address command ~command end of message bit.
{	
	TMR2 = 0x00;					//clear the TMR2 register before we start generating 38KHz Signal on the GPIO
	CCP1CONbits.CCP1MODE = 0xC;		//put The CCP module into PWM mode , the Duty is 50% and frequency is 38Khz as already set, 
	__delay_us(8999);				//wait for ~9ms this routine will delay for 8.999 ms + (3*500ns) = ~9ms (500ns is the instruction execution time at 8Mhz) and next instruciton will take 3 ins cycle to execute.
	CCP1CONbits.CCP1MODE = 0x0;		//turn off the CCP module stop generating 38KHz signal
	__delay_us(4490);				//wait for ~4.5ms  the value 4490 is compensated with the next instructions execution timing , it helps to keep precise timing. as described avobe
	
	sendByte(address);				//send address byte. (sendByte functions should not be called independently, only sendFrame should call it)
	sendByte(~address);				//send address logical invert 
	sendByte(command);				//send command
	sendByte(~command);				//send command logical invert
	
									//address and command is sent now send the end of message bit
	TMR2 = 0x00;					//clear the TMR2 register before we start generating 38KHz Signal on the GPIO
	CCP1CONbits.CCP1MODE = 0xC;		//Start generating 38KHz signal
	__delay_us(561);				//wait for ~562.5us again value 561 is compensated with the next instructions timing
	CCP1CONbits.CCP1MODE = 0x0;		//stop generating 38KHz signal.
	__delay_ms(40);					// wait for the Data Frame time. 
}

void sendByte(unsigned char byte)	// this function is called only by the sendFrame , to send each byte of data total 4bytes.
{
	unsigned char i;				//variable to hold the counter value
	for(i=8 ;i>0;i--)				//loop to send 8 individual bits.
	{	
		TMR2 = 0x00;				//clear the TMR2 register before we start generating 38KHz Signal on the GPIO
		CCP1CONbits.CCP1MODE=0xC;		//Start generating 38KHz signal
		__delay_us(561);			//wait for ~562.5us again value 561 is compensated with the next instructions timing
		CCP1CONbits.CCP1MODE=0x0;		//stop generating 38KHz signal
			
		if(byte & 0x80)				// as you have already noticed , this is example we send MSB first order. 
		{							// check for MSB bit if it is 1 then delay for 1.6875ms if it is zero then delay for 562.5us
		__delay_us(1686);			//delay for ~1.6875 ms again value 1686 is compensated with the next instructions timing
		}
		else
		{
		__delay_us(558);			//wait for ~562.5us again value 558 is compensated with the next instructions timing
		}
		byte = byte <<1;			//get the next lsb into msb (shift left the byte)
	}	
}

void sendRepeat()
{
	TMR2 = 0x00;					//clear the TMR2 register before we start generating 38Khz Signal on the GPIO
	CCP1CONbits.CCP1MODE = 0xC;		//Start generating 38KHz signal
	__delay_us(8999);				//wait for ~9ms 	
	CCP1CONbits.CCP1MODE = 0x0;		//stop generating 38KHz signal
	__delay_us(2245);				//wait for 2.25ms

	TMR2 = 0x00;					//clear the TMR2 register before we start generating 38Khz Signal on the GPIO
	CCP1CONbits.CCP1MODE = 0xC;		//Start generating 38KHz signal
	__delay_us(556);				//wait for ~562.5us
	CCP1CONbits.CCP1MODE = 0x0;		//stop generating 38KHz signal	
	__delay_us(96187);				//delay for 96.187 ms before sending the next repeat code
	
}