FORGET *RVASM*
ESP32


pub *RVASM*	PRINT" TAQOZ INTERACTIVE ASSEMBLER for RISC-V - 230628-0000" ;

{

		CPU		MHz	CM/MHZ	ROM	FLASH	RAM	USB	WiFi			MISC
====================================================================================================================================
ESP32-C3	RV32IMC		160		384K	QSPI	400K	Y	802.11bgn+BLE
ESP32-C6	RV32IMAC	160		320	QSPI	512K	Y	802.11bgn_ax 802.15.4
		RV32IMAC	20 LP				16K
RP2350		RV32IMAC+ x2	150>378				520K	Y	-		-
CH32V003	RV32E		48		1920	16K	2K					up to 5V operation & cheap
CH32V203	RV32		144		28K	224K	64K	2.0x2
CH32V307	RV32IMAFC	144		28K	480K	128K	2.0x3				SDIO Gigabit MAC+10M_PHY
GD32VF103	RV32IMAC	108			128K	32K
BL602		RV32IMAFCV	192 	3.1	128K	opt	276K		802.11bgn/BLE		SDIO
BL808		RV64IMAFCV			z				802.11bgn/BT+BLE+ZIG	NPU DSI DPI DBI audio video
		RV32
K210		RV64IMAFDC x2	400				8M


EXTENSIONS
    RV32E: 32bits architecture, with 16 general purpose registers
    RV32I: 32bits architecture, with 32 general purpose registers
    RV64I: 64bits architecture, with 32 general purpose registers
    M: Integer Multiplication and Division instructions
    C: Compressed Instructions as 16bits Encoding to reduce code size
    A: Atomic Instructions
    F: Single-Precision Floating-Point Instructions
    D: Double-Precision Floating-Point Instructions
    P: Packed-SIMD Instructions
}

\ ALIAS pri #define




	*** REGISTERS ***

: x0	0 ;	: x1	1 ;	: x2	2 ;	: x3	3 ;
: x4	4 ;	: x5	5 ;	: x6	6 ;	: x7	7 ;
: x8	8 ;	: x9	9 ;	: x10	10 ;	: x11	11 ;
: x12	12 ;	: x13	13 ;	: x14	14 ;	: x15	15 ;
: x16	16 ;	: x17	17 ;	: x18	18 ;	: x19	19 ;
: x20	20 ;	: x21	21 ;	: x22	22 ;	: x23	23 ;
: x24	24 ;	: x25	25 ;	: x26	26 ;	: x27	27 ;
: x28	28 ;	: x29	29 ;	: x30	30 ;	: x31	32 ;


: x0 zero ;
: x1 ra ;	--- return addr
: x2 sp ;	--- stack ptr
: x3 gp ;	--- global ptr
: x4 tp ;	--- thread ptr

: x5 t0 ;
: x6 t1 ;
: x7 t2 ;

: x8 s0 ;
: x9 s1 ;
: x10 a0 ;
: x11 a1 ;
: x12 a2 ;
: x13 a3 ;
: x14 a4 ;
: x15 a5 ;
: x16 a6 ;
: x17 a7 ;
( x18..x27 = s2..s11 )
( x28..x31 = t3..t6 )

\ x14 := tos,a,b,c,d,xx,yy,zz,r0,r1,r2,r3,r4,acc,index,limit,doloop


	 *** fixed data registers ***
x14			:= tos
x15			:= a
x16			:= b
x17			:= c
x18			:= d

( TAQOZ temporary registers )
x5			:= xx
xx 1+			:= yy
xx 2+			:= zz
yy			:= r0
zz			:= r1

r1 1+			:= r2
r2 1+			:= r3
r3 1+			:= r4
xx 1-			:= acc
acc			:= ac

x18			:= index
x19			:= limit
x20			:= doloop



private
byte warnings
pri WARN			warnings C++ ;


private
byte _cdepth
byte _adepth --- depth of stack at start of line
pri ADEPTH ( -- depth )		DEPTH _adepth C@ - ;




long opcode

pri op			opcode ! ;
pri +op			<< opcode @ OR opcode ! ;
pri fnc3		7 AND 12 +op ;
pri fnc7		$0F AND 25 +op ;
pri rd			$1F AND 7 +op ;
pri rs1			$1F AND 15 +op ;
pri rs2			$1F AND 20 +op ;
pri imm12		$FFF AND 20 +op ;
pri imm20		$FFFFF AND 12 +op ;


byte _pass

	*** LISTING ***

{

Listing format 4:7:3:9:9
F600_1A0B
F 30 0 0D 0B
}

private
long _pc
pub PC			_pc @ ;
pub codeorg ->		_pc ! ;

byte _l --- list control flag
pri LIST?				_l C@ ;
pri ?CRLF				LIST? 0EXIT CRLF ;
pri ?EMIT				LIST? IF EMIT ELSE DROP THEN ;
pri INDENT				CRLF _l C@ 0EXIT 16 SPACES ;
pri .ASM				LIST? IF CR PC DUP 16>> .H .WORD PRINT"  " .LONG ELSE DROP THEN ;
pri ASM!				opcode @
pri ASM,	 			DUP .ASM PC ! 4 _pc +! ;
pri ASMC,				DUP .ASM PC W! 2 _pc +! ;




	*** INSTRUCTIONS ***

( op rd,rs1,rs2/imm )

--- R TYPE


	*** LOGIC ***

	( REGISTER OPS )
	( rd rs1 rs2 fnc7 fnc3 -- )
pri Rop		%0110011 op fnc3 fnc7 rs2 rs1 rd ASM! ;

pub add ->		0 0 Rop ;
pub sub ->		$20 0 Rop ;
pub xor ->		0 4 Rop ;
pub or ->		0 6 Rop ;
pub and ->		0 7 Rop ;
pub sll ->		0 1 Rop ;
pub srl ->		0 5 Rop ;
pub sra ->		$20 5 Rop ;
pub slt ->		0 2 Rop ;
pub sltu ->		0 3 Rop ;


	( IMMEDIATE OPS )
 ( rd rs1 imm fnc3 -- )
pri Iop		%0010011 op fnc3 imm12 rs1 rd ASM! ;

--- addi x1,x1,4
pub addi ->		0 Iop ;
pub xori ->		4 Iop ;
pub ori ->		6 Iop ;
pub andi ->		7 Iop ;
pub slli ->		1 Iop ;
pub srli ->		5 Iop ;
pub srai ->		$20 5 << + 5 Iop ;
pub slti ->		2 Iop ;
pub slitu ->		3 Iop ;


	( LOAD/STORE OPS )
pri ldop	fnc3 %0000011 + SWAP imm12 + SWAP rs1 + SWAP rd + ASM! ;

--- lb x1,4(x2) --> lb x1 4 x2     lb x1,x2,4
--- $21283 = lh x5,0(4)

pub lb ->		0 ldop ;
pub lh ->		1 ldop ;
pub lw ->		2 ldop ;
pub lbu ->		4 ldop ;
pub lhu ->		5 ldop ;


--- LUI [31:12]
pub lui -> pri lui,	%0110111 op imm20 rd ASM! ;
pub auipc ->		%0010111 op imm20 rd ASM! ;

--- la	x15,_data1
pub la ->		2DUP 12 >> lui 12 bits lw ;
pub li ->
	DUP 12 >> ?DUP IF 3rd SWAP lui, OVER SWAP $FFF AND 0 Iop
	ELSE x0 SWAP 0 Iop THEN ;

pub mv ->		0 0 Iop ;
pub not	->		-1 4 Iop ;


pri immB
	2/ DUP $0F AND 8 +op 4 >> DUP $3F AND 25 +op
	6 >> DUP 1 AND 7 +op 2/ 1 AND 31 +op
	;

( BRANCH OPS ) --- beq rs1,rs2,label
 ( rs1 rs2 imm fnc3 -- )
pri Bop		%1100011 op fnc3 immB rs1 rs2 ASM! ;
pub beq	->	0 Bop
pub bne ->	1 Bop
pub blt	->	4 Bop
pub bge ->	5 Bop
pub bltu ->	6 Bop
pub bgeu ->	7 Bop

pri FLIP	-ROT SWAP ROT ;
pub ble -> 	FLIP 5 Bop ;
pub bleu ->	FLIP 7 Bop ;
pub bgt ->	FLIP 4 Bop ;
pub bgtu ->	FLIP 6 Bop ;

pub djnz -> ( rn,lab )	OVER DUP -1 zero Iop  zero SWAP 1 Bop ;

pri immJ
	2/ $FFFFF AND
	DUP $3FF AND 21 +op
	10 >> DUP 1 AND 20 +op
	2/ DUP $FF AND 12 +op
	8 >> 1 AND 31 +op
	;

pub jal -> ( rd,lab )		%1101111 op immJ rd ASM! ;
pub jalr -> ( rd,rs1,lab )	%1100111 op 0 fnc3 imm12 rs1 rd ASM! ;
pub jmp -> ( lab )		%1101111 op immJ zero rd ASM! ;


--- RV32M MULTIPLY EXT ---
( RV32M: Rop ( rd rs1 rs2 fnc3 -- )
pri RV32M			1 SWAP Rop ;
pub mul -> ( rd = (rs1 * rs2)	0 RV32M ;
pub mulh -> ( rd = (rs1 * rs2)	1 RV32M ;
pub mulsu -> ( rd = (rs1 * rs2)	2 RV32M ;
pub mulu -> ( rd = (rs1 * rs2)	3 RV32M ;
pub div -> ( rd = rs1 / rs2)	4 RV32M ;
pub divu -> ( rd = rs1 / rs2)	5 RV32M ;
pub rem -> ( rd = (rs1 * rs2)	6 RV32M ;
pub remu -> ( rd = (rs1 * rs2)	7 RV32M ;


--- RV32A ATOMIC ---
( RV32A: rd rs1 rs2 fnc5 -- )
pri RV32A			%011111 op 2 fnc3 2 << 3 OR fnc7  rs2 rs1 rd ASM! ;
pub lr.w ->			2 RV32A ;
pub sc.w ->			3 RV32A ;
pub amoswap.w ->		1 RV32A ;
pub amoadd.w ->			0 RV32A ;
pub amodand.w ->		$0C RV32A ;
pub amoor.w ->			$0A RV32A ;
pub amoxor.w ->			$04 RV32A ;
pub amomax.w ->			$14 RV32A ;
pub amomin.w ->			$10 RV32A ;


	--- RV32C COMPRESSED ---
( rd rs1 rs2 fnc7 fnc3 )
pri RVCR	;
pri RVCI ( rd imm  op fnc3)	13 << OR  ;

pub ret 			$8082 ASMC, ;

---				op  fnc3
pub c.lwsp			%10 %010 RVCI ;		--- c.lswp  rd imm sp )
pub c.li			%01 %010 RVCI ;		--- c.li  rd imm6
pub c.lui			%10 %010 RVCI ;
pub c.addi			%10 %010 RVCI ;
pub c.addi16sp			%10 %010 RVCI ;



pub c.swsp			%10 %010 RVCSS ;
pub c.lw			%10 %010 RVCL ;
pub c.sw			%10 %010 RVCS ;

pub c.j				%10 %010 RVCJ ;
pub c.jal			%10 %010 RVCJ ;
pub c.jr			%10 %010 RVCR ;
pub c.jalr			%10 %010 RVCR ;
pub c.beqz			%10 %010 RVCB ;
pub c.bnez			%10 %010 RVCB ;


pub c.addi4spn			%10 %010 RVCIW ;
pub c.slli			%10 %010 RVCI ;
pub c.srli			%10 %010 RVCB ;
pub c.srai			%10 %010 RVCB ;
pub c.andi			%10 %010 RVCB ;
pub c.mv			%10 %010 RVCR ;
pub c.add			%10 %010 RVCR ;
pub c.and			%10 %010 RVCS ;
pub c.or			%10 %010 RVCS ;
pub c.xor			%10 %010 RVCS ;
pub c.sub			%10 %010 RVCS ;
pub c.nop			%10 %010 RVCI ;
pub c.ebreak			%10 %010 RVCR ;

{
	...

000007a8 <Dictionary_+>:
     7a8:	0808                	addi	x10,x2,16
     7aa:	0000                	unimp
     7ac:	0662                	slli	x12,x12,0x18
     7ae:	0000                	unimp
     7b0:	2b01                	jal	cc0 <Dictionary_irq-fault>

     7ba:	00040737          	lui	x14,0x40
     7be:	41370713          	addi	x14,x14,1043 # 40413 <BIT18+0x413>
     7c2:	00f407b7          	lui	x15,0xf40
     7c6:	43378793          	addi	x15,x15,1075 # f40433 <BIT23+0x740433>

  li x14, 0x00000013 | reg_tos << 7  | reg_tos << 15                  # addi x8, x8, ...
  li x15, 0x00000033 | reg_tos << 7  | reg_tos << 15 | reg_tmp1 << 20 # add  x8, x8, x15


0100 0000 1001 1100
		409C			c.lw
		0491
		943E
		8082

pri stop		%0100011
--- sb	x1,4(x2)
pub sb ->		0 stop ;
}


{
	*** BRANCHING MACROS ***

pub FOR: ( n reg -- addr )
	-> ADEPTH IF 2DUP <> IF SWAP mv ELSE 2DROP THEN THEN PC ;
pub NEXT: ( reg addr -- ) 	djnz ;
pub BEGIN: ( -- pc )		PC ;
pub AGAIN: ( pc -- )		jmp ;
pub UNTIL: ( pc <cond> -- )	jmp ;	( {NZ} UNTIL, )
}

{
	--- simple labels ---

8 longs _locals
byte _lp
pri @l			_lp C@ 4* _locals + ;

pri label: ( index -- )
 	_lp C!
	@l @ IF
---	go back and resolve forward reference (works for djnz at present)
	@l @ C@ $FF = IF PC @l @ 4+ - 4/ $1FF - @l @ +! THEN
	ELSE PC @l ! THEN
	;

---			get address or use PC and set if not defineds
pri @label		_lp C! @l @ IF @l @ ELSE PC DUP @l ! THEN

pub .l0				0 label: ;
pub l0				0 @label ;
pub .l1				1 label: ;
pub l1				1 @label ;
pub .l2				2 label: ;
pub l2				2 @label ;
pub .l3				3 label: ;
pub l3				3 @label ;
pub .l4				4 label: ;
pub l4				4 @label ;
pub .l5				5 label: ;
pub l5				5 @label ;
pub .l6				6 label: ;
pub l6				6 @label ;
pub .l7				7 label: ;
pub l7				7 @label ;

256 bytes labels
pri +LABEL ( str )		labels DUP LEN$ + $! ;

pub :: 	( <label> -- )		GET$ +LABEL ;
}


	*** TAQOZ KERNEL ***

{
pub EXIT;		AT 0EXIT 2+ jmp, ;
pub >POPX		AT DROP CALL\, ;
pub DROP;		AT DROP JMP\, ;
pub 2DROP;		AT 2DROP JMP\, ;
pub 3DROP;		AT 3DROP JMP\, ;

pub >PUSHX		AT DUP 1+ CALL\,  ;
pub >ROT		AT ROT CALL\, ;
pub >SWAP		AT SWAP CALL\, ;
pub >I			AT I CALL\, ;
pub >SPIRD		AT SPIRD CALL\, ;
pub >SPIWB		AT SPIWB CALL\, ;
pub >SPICE		AT SPICE CALL\, ;
}

	*** INPUT, PARSING, & EVALUATION ***



long nx'	--- eval string ptr
byte nxrd
word ukey1
--- EVAL KEY INPUT (via linebuf normally)
pri EVALKEY	nx' @ nxrd C@ + C@ DUP IF nxrd C++ ELSE ukey1 W@ ukey W! THEN ;

512 bytes symbols
{
Simple symbol table - entry[16] = string[11],0,long
Use: 0 SYMBOL  to clear table
always returns symbol table address and a the string addr if not first time
else a false flag
}

pri @TYPE		11 + ;
pri @LINK		12 + ;
pri ABS!		DUP @ $FFFFF ANDN ROT OR SWAP ! ;

pri OLDLABEL ( labflg str sym )
\  	."  OLD "
	  NIP SWAP IF ( sym ) --- refd and now can be resolved --- assume single ref for now
\ 	."  +RESOLVE"
	  PC SWAP @lINK @ ABS!
	  ELSE --- redf again ( need to allow linking )
\ 	  ."  AGAIN"
	  @lINK @ EXIT
	  THEN
	  ;

pri NEWLABEL ( labflg str sym )
\	."  NEW "
--- NEW LABEL - append to symbol table
	SWAP OVER $! ( labflg sym )
 	SWAP ( sym labflg )
---	NEW LABEL - in label column or forward reference?
	  IF ( new label )
\	    ." LABEL "
	    'L' OVER @TYPE C!
	    PC SWAP @LINK !
	  ELSE
\	    ."  REFERENCE "
	    'F' OVER @TYPE C!
	    PC OVER @lINK ! PC
	  THEN
	;

pri SYMBOL ( str -- symptr str/false ) ( 0 -- )
	?DUP IF
	  DUP LEN$ nxrd C@ 2- => SWAP ( labflg str )
	  symbols ( labflg str sym )
	  BEGIN DUPC@
	  WHILE 2DUP $= IF OLDLABEL EXIT THEN
	    16 +
	  REPEAT ( labflg str sym)
	  NEWLABEL
	ELSE
	  symbols 512 ERASE
	THEN
	;


pub CALL$ ( string -- )
	DUP C@
	IF DUP SEARCH ( str nfa )
	  ?DUP IF NIP CFA CALL
	  ELSE ( str ) SYMBOL
	  THEN
	ELSE DROP
	THEN
	;
{
treat unknown "words" by pulling apart and  transposing operators
rp,stacks+$20  -->  rp stacks $20 +
}
byte cflg
pri EVALUATE ( str -- )
	cflg C~
---	pad out with a space
	$20 OVER DUP LEN$ + W!
---	set string ptrs
	nx' ! nxrd C~
--- point to EVALKEY input
	ukey W@ ukey1 W! ' EVALKEY ukey W!
	BEGIN GET$ DUP LEN$
    WHILE cflg C@
      IF SEARCH CFA cflg C~
      ELSE NUMBER 0= IF @WORD CALL$ THEN  THEN
    REPEAT
	DROP
	;


--- @ is replaced with ''
pri  ''	cflg C~~ ;		\ GET$ SEARCH DUP IF CFA CALL THEN ;


byte keywr
128 bytes linebuf

pri PUTCH	linebuf keywr C@ + W! keywr C++ ;
pri PUT$	BEGIN DUP C@ ?DUP WHILE PUTCH 1+ REPEAT DROP ;
pri BSCH	keywr C-- ;





pri ASMKEY
	SWITCH
	$1B 	CASE " end"  EVALUATE !SP CONSOLE BREAK
	^X 	CASE linebuf PRINT$ $0D SWITCH BREAK
	^H 	CASE keywr C@ IF ^H EMIT SPACE ^H EMIT BSCH 0 PUTCH BSCH THEN BREAK
\	'#' 	CASE " #s " PUT$ '#' ?EMIT BREAK
	'['	CASE $20 PUTCH CASE@ ?EMIT BREAK
	']'	CASE $1F ANDN OVER %1100000 AND IF $1F AND THEN $3F AND + BREAK
---	skip # comments
	'#' 	CASE CASE@ ?EMIT BEGIN WKEY DUP ?EMIT $0D = UNTIL $0D SWITCH THEN

	',' 	CASE $20 PUTCH CASE@ ?EMIT BREAK
	'@' 	CASE " '' " PUT$ CASE@ ?EMIT BREAK
---	convert tab to a space
	^I 	CASE $20 PUTCH ^I ?EMIT BREAK
	$20 	CASE CASE@ DUP ?EMIT PUTCH BREAK
	$0D 	CASE keywr C@ 1 > IF $20 PUTCH $0D PUTCH THEN BREAK
	CASE@ $20 < ?EXIT
---	otherwise just echo and buffer character
	CASE@ DUP ?EMIT PUTCH
	;
pri TXT>ASM ( str1 -- str2 )
	keywr C~
	DUP LEN$ ADO I C@ ASMKEY LOOP
;


pub (END)
	FORTH \ OFF TIA ( hide assembler lexicon )
---	EXIT               fix up compilation ptr
	$4D PC DUP @HERE ! DUP uhere 4+ ! W!
	10 flags CLR unum W~
	DEPTH _cdepth C@ <> IF PRINT"   Stack mismatch! " WARN THEN
	warnings C@ ?DUP IF . PRINT"  warnings " THEN
	CR
	;

pub LIST ( on/off -- )			_l C! ;

pub FORGET$ ( str -- )	SEARCH ?DUP IF DUP DUPC@ + 3 + names ! DUPC@ + 1+ W@ DUP uhere !  ' EXIT SWAP W! THEN ;

pub PASS1
	_locals 32 ERASE _pass C~
	ON LIST
	flags C@ $0F = IF OFF LIST THEN
	warnings C~ DEPTH 1- _cdepth C!
	" SYMBOLS" FORGET$
	symbols C@ 3 = IF 0 SYMBOL THEN
---	Compile ASM code to call following assembly code, then stop compiling
\	[C] [W] [C] [
---	use as asm/end flag
	10 flags SET
---	use code memory for assembly but keep temp code out of the way
\	HERE _pc !  BUFFERS @HERE W!
---	check for symbols before searching dictionary
	AT SYMBOL unum W!
	BEGIN GETLINE keywr C@ 1 > IF EVALUATE EOL THEN  10 flags SET? 0= UNTIL
	;



RECLAIM *RVASM*

--- Create a new vocabulary that spans from here back to the start of the assembler
VOCAB RVASM *RVASM*
--- Now reset the vocab stack to Forth and then switch to ASSEMBLER vocab
FORTH RVASM

{
TAQOZ# NFA' CODE QD ---
---                      ---LINK FIELD---
0B229: 84 43 4F 44  45 98 84 00  80 6B C4 47  47 45 54 4C     '.CODE....k.GGETL'
0B239: 49 4E 45 70  84 45 44 4F  4B 45 59 88  83 44 42 53     'INEp.EDOKEY..DBS' ok

                   CON ' fflags bit fields '
                   	echoF	= 0
                   	okF	= 1		' disable OK response'
                   	ackF	= 2		' --- response inhibit '
                   	lineF	= 3		' disable newline '
                   	ipF	= 4             ' interpret this number in IP format where a "." separates bytes
                   	signF	= 5
                   	compF	= 6
                   	defF	= 7
                   	passF	= 8
}


( create private constant in the dictionary for all symbols that end in a : )

long _pc

: PC	_pc @ ;
--- Hack a version of := that accepts a string rather than getting one
: +LABEL ( val str -- )		NOP NOP NOP NOP NOP NOP NOP NOP ;
' := ' +LABEL 16 CMOVE     2 ' +LABEL 2+ W+!

: DEF?  7 flags SET? ;
: NEWLABEL	_pass C@ 0= IF @WORD DUP LEN$ + 1- C~  @WORD 1- C-- PC  @WORD +LABEL THEN ;
: LABEL?	@WORD DUP LEN$ + 1- C@ ':' = DEF? 0= AND IF  NEWLABEL TRUE ELSE FALSE THEN ;
' LABEL? unum W!



pub end 	(END) ;

pre RVASM	' LABEL? unum W! _pass C~ _pc ~ AT (CODE) RVASM: ;


END
