ESP32
( meter )
--- added larger capture buffer and clipping for narrower terminals
: *METER*	PRINT"  Forth Meter 250321-1300 " ;
: mv		3000 4095  */ 24 + ;
: mv@ 		ADC mv ;
: METER		5 0 DO CR I PRINT ." : " I mV@ 4 U.R PRINT" mV" LOOP ;
128 := capsz  capsz 2* 5 * bytes caps
: CAP5		capsz 0 DO  5 0 DO I ADC   I capsz 2* * caps + J 2* + W! LOOP LOOP ;
: avgmv@	0 SWAP capsz 2* * caps + capsz 2* BOUNDS DO I W@ + 2 +LOOP capsz / mv ;
: CLEAN		8 0 DO SPACE 8 EMIT $0A EMIT LOOP 8 UP ;
CREATE VCH    '_' _ '-' _ '=' _ '"' _
: .PIX  $FFF & 7 >> CLEAN 7 OVER 2 >> - DOWN DUP 3 AND VCH + C@ EMIT 7 SWAP 2 >> - UP ;
: capped	_xmax W@ 44 - 1 MAX capsz MIN 2* ; ( cap capture display to terminal width )
: .CAP		capsz 2* * caps + capped BOUNDS DO I W@ .PIX 2 +LOOP ;
: .METERS
	5 0 DO
	  CAP5 CR plain yellow pen BIG I PRINT 3 DOWN CON '=' EMIT
	  3 UP plain blue paper bold I avgmv@ BIG 3 .DEC 'V' EMIT
	  CON plain SPACE I 1+ bold pen I .CAP
	  7 DOWN CR LOOP
;
: METERS
	OFF CURSOR CLS 9 EMIT bold cyan pen blue paper *METER*
	BEGIN HOME CR .METERS 200 ms KEY? UNTIL ON CURSOR
	;

*END*

