ESP32
: *C3*		." some C3 relevant extensions " ;


$50000000 	:= 	RTCRAM  		--- $2000
$3FC80000 	:= 	SRAM 			--- $70000 first 64k protected
SRAM 64 KB +	:=	URAM


$6000_0000	:=	UART0
\ $6000_1000	:=	Reserved
$6000_2000	:=	SPI1
$6000_3000	:=	SPI0
$6000_4000	:=	GPIO
\ $6000_5000	:=	Reserved
$6000_7000	:=	TIMER
$6000_8000	:=	LPM				--- Low-Power Management
$6000_9000	:=	IOMUX
\ $6000_A000	:=	Reserved
$6001_0000	:=	UART1
\ $6001_1000	:=	Reserved
$6001_3000	:=	I2C
$6001_4000	:=	UHCI0
\ $6001_5000	:=	Reserved
$6001_6000	:=	RCP				--- Remote Control Peripheral
\ $6001_7000	:=	Reserved
$6001_9000	:=	LEDPWM
$6001_A000	:=	eFuse
\ $6001_B000	:=	Reserved
$6001_F000	:=	Timer0
$6002_0000	:=	Timer1
\ $6002_1000	:=	Reserved
$6002_3000	:=	SYSTIMER
	SYSTIMER 	:= STCON0
	SYSTIMER 4 +	:= STOPR0
	SYSTIMER $0C +	:= STLOH0
	SYSTIMER $10 +	:= STLOL0
	SYSTIMER $40 +	:= STHI0
	SYSTIMER $44 +	:= STLO0
	SYSTIMER $5C +	:= STSYN0
$6002_4000	:=	SPI2
\ $6002_5000	:=	Reserved
$6002_6000	:=	APB
\ $6002_7000	:=	Reserved
$6002_B000	:=	TWAI 				--- Two-wire Automotive Interface
\ $6002_C000	:=	Reserved
$6002_D000	:=	I2S
\ $6002_E000	:=	Reserved
$6003_A000	:=	AES
$6003_B000	:=	SHA
$6003_C000	:=	RSA
$6003_D000	:=	DIGSIG            	--- Digital Signature
$6003_E000	:=	HMAC            	--- 0x6003_EFFF4
$6003_F000	:=	GDMA          		--- 0x6003_FFFF4
$6004_0000	:=	ADC           		--- 0x6004_0FFF4
$6004_1000	:=	Reserved				        
$6004_3000	:=	USBDEV	 			--- Serial/JTAG Controller
\ $6004_4000	:= 	Reserved
$600C_0000	:=	SYSREGS				
$600C_1000	:=	TSENS				--- Sensitive Register
$600C_2000	:=	INTMAT				--- Interrupt Matrix
$600C_3000	:=	Reserved			--- 
$600C_4000	:=	CACHECFG			--- Configure Cache
$600C_C000	:=	MEMENC				--- External Memory Encryption and		--- 0x600C_CFFF4
$600C_D000	:=	Reserved			
$600C_E000	:=	DEBUG				--- Assist Debug
\ $600C_F000 	:=	Reserved
$600D_0000	:=	WORLDCON			--- World Controller




\ $4037C000 := RAM \ $64000
\ $4038E800 protected?

( bug in adc2 errata )
: @ADC ( reg -- a ) ADC + ;

: ADCCTL	0 @ADC ;
: ADCCTL2	4 @ADC ;
: ADCFIL1	8 @ADC ;
: ADCTAB1	$18 @ADC ;
: ADCTAB2 	$1C @ADC ;
: ADCONE	$20 @ADC ;
: ADCARB	$24 @ADC ;
: ADCFIL0	$28 @ADC ;
: ADCSTA1	$2C @ADC ;
: ADCSTA2	$30 @ADC ;
: ADCTHR0	$34 @ADC ;
: ADCTHR1	$38 @ADC ;
: ADCTHR	$3C @ADC ;
: ADCIE		$40 @ADC ;
: ADCINT	$44 @ADC ;
: ADCISR	$48 @ADC ;
: ADCICR	$4C @ADC ;
: ADCDMA	$50 @ADC ;
: ADCCLK	$54 @ADC ;
: ADCTEM	$58 @ADC ;
: ADCTEM2	$5C @ADC ;
: ADCCAL	$60 @ADC ;
: ADCVER	$3FC @ADC ;


( SYSTEM TIMER 16MHZ )

: @STR			$60023000 + ;
\ : SYSTIMER ( -- d )	$44 @STR @ $40 @STR @ ;
{
1. Set SYSTIMER_TIMER_UNITn_UPDATE to update the current count value into SYSTIMER_TIMER_UNITn_
VALUE_HI and SYSTIMER_TIMER_UNITn_VALUE_LO.
2. Poll the reading of SYSTIMER_TIMER_UNITn_VALUE_VALID, till it’s 1, which means user now can read the
count value from SYSTIMER_TIMER_UNITn_VALUE_HI and SYSTIMER_TIMER_UNITn_VALUE_LO.
3. Read the lower 32 bits and higher 20 bits from SYSTIMER_TIMER_UNITn_VALUE_LO and
SYSTIMER_TIMER_UNITn_VALUE_HI
}

REMEMBER
*END*
