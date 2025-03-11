( NEEDS EXTEND.FTH )
ESP32
: *RVSEE*	PRINT" RV32 DISASSEMBLER " ;

: -CI ;
: -CIW ;
: -CSS ;
: -CL ;
: -CS ;
: -CJ ;
: -CR ;
: -CB ;

: c.lwsp	%010 %10 -CI ;
: c.swsp	%110 %10 -CSS ;
: c.lw		%010 %00 -CL ;
: c.sw		%110 %00 -CS ;
: c.j		%101 %01 -CJ ;
: c.jal		%001 %01 -CJ ;
: c.jr		-CR ;
: c.jalr	-CR ;
: c.beqz	-CB ;
: c.bnez	-CB ;
: c.li		-CI ;
: c.lui		-CI ;
: c.addi	-CI ;
: c.addi16sp	-CI ;
: c.addi4spn	-CIW ;
: c.slli	-CI ;
: c.srli	-CB ;
: c.srai	-CB ;
: c.andi	-CB ;
: c.mv		-CR ;
: c.add		-CR ;
: c.and		-CS ;
: c.or		-CS ;
: c.xor		-CS ;
: c.sub		-CS ;
: c.nop		-CI ;
: c.ebreak	-CR ;

: RV32C


;

: disasm ( addr cnt -- )
	>A >A
	BEGIN
	  AW@+ DUP 3 & 3 <>
	  IF RV32C
	  ELSE
	  THEN

;




END
