ESP32
: *UART* 	;
{
A smart USB to serial interface

}

{
: GPIO		$6000_4000 + ;
: HIGH		MASK DUP $24 GPIO ! 8 GPIO ! ;
: LOW		MASK DUP $24 GPIO ! $0C GPIO ! ;


: HIGH		MASK DUP $6000_4024 ! $6000_4008 ! ;
: LOW		MASK DUP $6000_4024 ! $6000_400C ! ;
}

( peripherals constants builders )
: UART0		$6000_0000 + ;
: SPI1		$6000_2000 + ;
: SPI0		$6000_2000 + ;
: GPIO		$6000_4000 + ;
: LPM		$6000_8000 + ;
: IOMUX		$6000_9000 + ;

: UART1		$6001_0000 + ;
: I2C		$6001_3000 + ;
: UHCI0		$6001_4000 + ;
: RCP		$6001_6000 + ;
: LPC		$6001_9000 + ;
: EFC		$6001_A000 + ;
: TG0		$6001_F000 + ;

: TG1		$6002_0000 + ;
: ST		$6002_3000 + ;
: SPI2		$6002_4000 + ;
: SYSCON	$6002_6000 + ;
: TWAI		$6002_B000 + ;
: I2S		$6002_D000 + ;

: AES		$6003_A000 + ;
: SHA		$6003_B000 + ;
: RSA		$6003_C000 + ;
: DSIG		$6003_D000 + ;
: HMAC		$6003_E000 + ;
: GDMA		$6003_F000 + ;

: SARADC	$6004_0000 + ;
: USB		$6004_3000 + ;

: SYSREG	$600C_0000 + ;
: PMS		$600C_1000 + ;
: INTMAT	$600C_2000 + ;
: EXTCRY	$600C_C000 + ;
: ADEBUG	$600C_E000 + ;
: WORLDC	$600D_0000 + ;

$0010 SYSREG := PCLKEN0
$0014 SYSREG := PCLKEN1
$0018 SYSREG := PRSTEN0

$0004 UART1 := U1INTRAW
$001C UART1 := U1STAT
$0020 UART1 := U1CON0
$0024 UART1 := U1CON1
$0078 UART1 := U1CLKCON
$007C UART1 := U1DATE
$0080 UART1 := U1ID

$0004 UART0 := U0INTRAW
$001C UART0 := U0STAT
$0020 UART0 := U0CON0
$0024 UART0 := U0CON1
$0078 UART0 := U0CLKCON
$007C UART0 := U0DATE
$0080 UART0 := U0ID

\ 0 IOMUX :=

: INPSEL ( gpio -- adr )	4* $154 + GPIO ;
: OUTSEL ( gpio -- adr )	4* $554 + GPIO ;


: SETB ( bit addr -- )	DUP @ ROT MASK OR SWAP ! ;
: CLRB ( bit addr -- )	DUP @ ROT MASK INVERT AND SWAP ! ;
: SET? ( bit addr -- )	@ SWAP MASK AND 0<> ;
: CLR? ( bit addr -- )	@ SWAP MASK AND 0= ;

{
freq/clkdiv+(divfrac/16)
40000000/
}
: BAUD ( val n-- )
	SWAP 40,000,000 SWAP / 12 <<
	( diva 6 << divb OR )
	%1101_11 20 << OR
	SWAP IF U1CLKCON ELSE U0CLKCON THEN !
;

: SETUART1
	BEGIN 31 U1DATE CLR? UNTIL	--- wait for UART_REG_UPDATE to become 0, which indicates the completion of the last synchronization;
--- configure static registers (if any) following Section 26.5.1.2;
	921600 1 BAUD
--- select the clock source via UART_SCLK_SEL;
--- configure divisor of the divider via UART_SCLK_DIV_NUM, UART_SCLK_DIV_A, and UART_SCLK_DIV_B; NUM+(B/A)
--- configure the baud rate for transmission via UART_CLKDIV and UART_CLKDIV_FRAG;
	%000_10000000000000000000000_01_11_00 U1CON0 ! --- configure data length via UART_BIT_NUM;
--- configure odd or even parity check via UART_PARITY_EN and UART_PARITY;
--- optional steps depending on application ...
	31 U1ID SETB --- synchronize the configured values to the Core Clock domain by writing 1 to UART_REG_UPDATE.
;
: SETUART0
	BEGIN 31 U0DATE CLR? UNTIL
	921600 0 BAUD
	%000_10000000000000000000000_01_11_00 U0CON0 !
	31 U0ID SETB
;

: ENUART1
--- To enable UARTn transmitter:
\ U1CON1 ! --- configure TX FIFO’s empty threshold via UART_TXFIFO_EMPTY_THRHD;
--- disable UART_TXFIFO_EMPTY_INT interrupt by clearing UART_TXFIFO_EMPTY_INT_ENA;
--- write data to be sent to UART_RXFIFO_RD_BYTE;
--- clear UART_TXFIFO_EMPTY_INT interrupt by setting UART_TXFIFO_EMPTY_INT_CLR;
--- enable UART_TXFIFO_EMPTY_INT interrupt by setting UART_TXFIFO_EMPTY_INT_ENA;
--- detect UART_TXFIFO_EMPTY_INT and wait for the completion of data transmission.
--- To enable UARTn receiver:
--- configure RX FIFO’s full threshold via UART_RXFIFO_FULL_THRHD;
--- enable UART_RXFIFO_FULL_INT interrupt by setting UART_RXFIFO_FULL_INT_ENA;
--- detect UART_TXFIFO_FULL_INT and wait until the RX FIFO is full;
--- read data from RX FIFO via
;

: !UART1
	24 PCLKEN0 SETB		--- enable the clock for UART RAM by setting SYSTEM_UART_MEM_CLK_EN to 1;
	5 PCLKEN0 SETB		--- enable APB_CLK for UARTn by setting SYSTEM_UARTn_CLK_EN to 1
	5 PRSTEN0 CLRB		--- clear SYSTEM_UARTn_RST to 0;
	23 U1CLKCON SETB	--- write 1 to UART_RST_CORE;
	5 PRSTEN0 SETB		--- write 1 to SYSTEM_UARTn_RST;
	5 PRSTEN0 CLRB		--- clear SYSTEM_UARTn_RST to 0;
	23 U1CLKCON CLRB	--- clear UART_RST_CORE to 0.
	30 U1ID CLRB		--- enable register synchronization by clearing UART_UPDATE_CTRL.
	SETUART1
	ENUART1
	;

: !UART0
	24 PCLKEN0 SETB		--- enable the clock for UART RAM by setting SYSTEM_UART_MEM_CLK_EN to 1;
	5 PCLKEN0 SETB		--- enable APB_CLK for UARTn by setting SYSTEM_UARTn_CLK_EN to 1
	2 PRSTEN0 CLRB		--- clear SYSTEM_UARTn_RST to 0;
	23 U0CLKCON SETB	--- write 1 to UART_RST_CORE;
	2 PRSTEN0 SETB		--- write 1 to SYSTEM_UARTn_RST;
	2 PRSTEN0 CLRB		--- clear SYSTEM_UARTn_RST to 0;
	23 U0CLKCON CLRB	--- clear UART_RST_CORE to 0.
	30 U0ID CLRB		--- enable register synchronization by clearing UART_UPDATE_CTRL.
	SETUART0
	6 21 OUTSEL !		--- select TXD0 function for GPIO 21
	$1809 21 4* 4 + IOMUX !	--- Output en + drive etc
	;



*END*
