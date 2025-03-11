ESP32

: TAB					9 EMIT ;
: PRT$ ( n -- )			TAB <# # # $2E HOLD #S $24 HOLD #>  TYPE ;
VARIABLE @sub
VARIABLE @total
: .SUB					@sub @ PRT$ ;
: SELL ( qty rcd -- )	2DUP +! CELL+ @ * DUP @sub +! PRT$ .SUB ;
: SUB					TAB .SUB ;
: TOTAL					@sub @ @total +! 0 @sub ! ;
: TENDERED ( n -- )		DUP PRT$ CR ." SUBTOTAL"  SUB @sub @ - CR ." CHANGE" TAB TAB PRT$  TOTAL ;

VARIABLE @apples CELL ALLOT  25 @apples CELL+ !
VARIABLE @bananas CELL ALLOT 50 @bananas CELL+ !
VARIABLE @oranges CELL ALLOT 30 @oranges CELL+ !

:  APPLES ( n -- )  	@apples  SELL ;
:  BANANAS ( n -- )   	@bananas SELL ;
:  ORANGES ( n -- ) 	@oranges  SELL  ;

*END*

6 APPLES
3 BANANAS
4 ORANGES
SUB
10.00 TENDERED
