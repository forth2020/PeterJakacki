ESP32
--- LIFT CHALLENGE    102 lines and 1,286 bytes
\ FORGET *LIFT*

: *LIFT*        PRINT" Simple lift emulator " ;

0 bytes *lift*
: .TITLE        PRINT"   LIFT SIMULATOR - PBJ   "  ;
: .HEADER       PRINT"  * FORTH2020 * " ;

: >UPPER	DUP 'a' 'z' >< IF $20 - THEN ;

{
An exercise in controlling a lift in Forth and how to go about it

A lift has 32 levels including ground floor and basement 1, basement 2

If a lift is going up it needs to know if it has to stop at a floor (dest or up source)
An encoder feeds back the position of the lift including in-between floors

https://tinyurl.com/LiftChallenge

CONVENTIONS:
!THING means Init or Set THING
?THING means Check or poll THING
THING? returns a flag
THING@ fetches a value
[THING] means button for THING
_thing in lowercase is a variable (byte,long,other)
#thing is a constant value

NOTES:
?! is the simple case switch set
?= compares the stack with the switch that was set
}


64 value #max
16 value #floors

( button counter flags - skip 0, use from floor #1 )
#max 1+ bytes _ups
#max 1+ bytes _downs
#max 1+ bytes _gotos

byte _floor     --- current floor for lift cab
byte _lift      --- lift status - up/down
byte _dwell     --- dwell timer
byte _dwells    --- dwell multiplier
long _num       --- entry number

( DIGIT ENTRY )
: CLEAR                 _num ~ ;
: +NUM ( ch -- )        '0' - _num @ 10 * + _num ! ;
: NUM@                  _num @ CLEAR ;

: DING 		        7 EMIT ;
: DWELL                 6 _dwell C! DING ;
: DWELL?                _dwell C@ ;

: FLOOR@                _floor C@ ;
: BOTTOM?               FLOOR@ 1 = ;
: TOP?                  FLOOR@ #floors = ;
: lift=                 _lift C@ 7 AND = ;

: GOING-UP?             1 lift= ;
: GOING-DOWN?           2 lift= ;
: IDLE?                 0 lift= ;
: PARK                  0 _lift C! ;
: GO-UP                 1 _lift C! ;
: GO-DOWN               2 _lift C! ;
: DOWN? ( flr -- f )    _downs + C@ ;
: UP? ( flr -- f )      _ups + C@ ;
: GOTO? ( flr -- f )    _gotos + C@ ;
: REQUEST? ( flr -- f ) DUP DOWN? OVER UP? + SWAP GOTO? + ;
: GOTOS? ( -- cnt )     0 #floors 1+ 1 DO I GOTO? + LOOP ;
: REQUESTS? ( -- cnt )  0 #floors 1+ 1 DO I REQUEST? + LOOP ;
: LIFT? ( flr -- flg )  FLOOR@ = ;

        ( *** DISPLAY *** )
: EMOJI         	$F0 EMIT $9F EMIT DUP 6 >> $98 + EMIT $3F AND $80 + EMIT ;

: PERSON		$3CD  EMOJI ;
: [PEOPLE]		'[' EMIT PERSON ']' EMIT ;
: PEOPLE		IF PERSON PERSON THEN ;

: TILE                  '_' EMIT ;
: TILES                 '_' SWAP EMITS ;

: ?EMOJI		SWAP IF EMOJI ELSE DROP 2 TILES THEN ;
: .UP			$209 ?EMOJI ;
: .DN			$20B ?EMOJI ;
: .GO			$1E0 ?EMOJI ;
: .[UP]			PRINT" [ UP]" ;
: .[DN]			PRINT" [ DN]" ;

: CAB ( floor -- )
    red PAPER BOLD
    DWELL? IF green PAPER 5 TILES EXIT THEN
    GOING-UP? IF GOTOS? IF [PEOPLE] ELSE .[UP] THEN EXIT THEN
    GOING-DOWN? IF GOTOS? IF [PEOPLE] ELSE .[DN] THEN EXIT THEN
    IDLE? IF PRINT" [---]" EXIT THEN
;

: WALL		        PLAIN BOLD '|' EMIT PLAIN ;
: DOOR ( flr -- )       LIFT? DWELL? AND IF TILE ELSE WALL THEN PLAIN ;
: CABLE	        	FLOOR@ > IF 5 PEN ."   |  " ELSE 5 SPACES THEN PLAIN ;

: WELL ( floor -- floor )
    WALL
    DUP LIFT? IF CAB ELSE DUP CABLE THEN
    DUP DOOR
;

: TITLE                 HOME  BOLD REVERSE .TITLE PLAIN ;
: ROOF                  CR '=' 25 EMITS ;
: WINCH
        CR PRINT"    =[" FLOOR@ 1 & IF 'x' ELSE '+' THEN EMIT  PRINT" ]="
        SPACE BLINKING yellow PEN magenta PAPER
        .HEADER PLAIN UL SPACE PLAIN
;

: INDICATORS ( floor -- floor )
    $08 16 EMITS
    PLAIN blue PAPER
    DUP UP? .UP
    DUP DOWN? .DN
    DUP GOTO? .GO
    DUP UP? OVER DOWN? OR PEOPLE
    PLAIN
;
: FLOOR ( floor -- floor )
--- 16|  |  |_______________|
    CR DUP green PAPER 2 U.R PLAIN
    WELL
    PLAIN blue PAPER 15 TILES PLAIN WALL
;
: FLOORS        #floors DUP 0 DO FLOOR INDICATORS 1- LOOP DROP ;

: SHOW
        TITLE
        ROOF
        WINCH
        FLOORS
        CR '=' 25 EMITS+
        CR _num @ ?DUP IF . ELSE 8 SPACES THEN
;

        ( *** BUTTON ACTIONS *** )

( UP DOWN buttons on each floor )
: UP ( floor -- )               _ups + C++ ;
: DOWN ( floor -- )             _downs + C++ ;
: GO ( floor -- )               _gotos + C++ ;
: STEP
        GOING-UP? IF FLOOR@ #floors <> IF _floor C++ SHOW THEN THEN
        GOING-DOWN? IF FLOOR@ 1 <> IF -1 _floor C+! SHOW THEN THEN
;
{
If lift is idle and a button is pressed - priority is:
1. lift button first
2. closer floors next
}
: ?FLOOR        FLOOR@ GOTO? IF FLOOR@ _gotos + C~ DWELL THEN ;
: ?UPS          GOING-DOWN? 0= FLOOR@ UP? AND IF FLOOR@ _ups + C~ DWELL THEN ;
: ?DOWNS        GOING-UP? 0= FLOOR@ DOWN? AND IF FLOOR@ _downs + C~ DWELL THEN ;
: ?GO-UP
        GOING-DOWN? ?EXIT
        TOP? ?EXIT
        #floors 1+ FLOOR@ 1+ DO I UP? I GOTO? OR IF GO-UP STEP LEAVE THEN LOOP
;
: ?GO-DOWN
        GOING-UP? ?EXIT
        BOTTOM? ?EXIT
        0 FLOOR@ 1- ?DO I DOWN? I GOTO? OR IF GO-DOWN STEP LEAVE THEN -1 +LOOP
;
: ?IDLE
        IDLE? 0EXIT
        #floors 1+ FLOOR@ 1+ ?DO I DOWN? IF GO-UP STEP LEAVE THEN LOOP
        IDLE? 0EXIT
        BOTTOM? ?EXIT
        0 FLOOR@ 1- ?DO I UP? IF GO-DOWN STEP LEAVE THEN -1 +LOOP
;
: COMPUTE
        REQUESTS? 0EXIT
        ?FLOOR ?UPS ?DOWNS
	_dwell C@ 0= IF PARK ?GO-UP ?GO-DOWN ?IDLE THEN
;


: ?[CLEAR]	$08 ?= IF CLEAR CLS SHOW THEN ;
: ?[REFRESH]	$20 ?= IF COMPUTE SHOW THEN ;

( BUTTONS IN CAB - console mode - enter number then GO )
: ?[0..9]	'0' '9' ?>< IF ?@ +NUM THEN ;
: ?[GO]		'G' ?= $0D ?= OR IF NUM@ GO SHOW THEN ;

( FLOOR BUTTONS - UP & DOWN )
: ?[UP]		'U' ?= iF NUM@ UP SHOW THEN ;
: ?[DOWN]	'D' ?= IF NUM@ DOWN SHOW THEN ;

( CHANGE LIFT PARAMETERS )
: ?[DWELL]	'*' ?= IF NUM@ _dwells C! THEN ;
: ?[FLOORS]	'#' ?= IF NUM@ #max MIN to #floors CLS SHOW THEN ;
: ?[SLOW]	'S' ?= IF 20 _dwells C! THEN ;
: ?[FAST]	'F' ?= IF 1 _dwells C! THEN ;

                ( CONSOLE BUTTONS )
{
This "Check Buttons" routine are polled within the dwell delay loop
It will capture serial console keys and process digits or single commands.
Pressing 1 2 3 will simply builds 123 which can be read by NUM@
1U request an up lift from floor 1
6G go to the 6th floor from lift
}

: ?[BUTTONS]
    KEY?
    IF KEY >UPPER ?!
      ?[CLEAR]
      ?[0..9]
      ?[GO]
      ?[UP]
      ?[DOWN]
        ( ENGINEERING CONSOLE  )
      ?[DWELL]
      ?[SLOW]
      ?[FAST]
      ?[FLOORS]
      ?[REFRESH]
    THEN
;

: SCAN
      SHOW
---    to dwell - or not to dwell or pause or just compute
       DWELL? IF _dwell C-- ELSE $20 ?= 0= IF COMPUTE THEN THEN
---   just check buttons while dwelling
      _dwells C@ ?DUP IF 0 DO ?[BUTTONS] 20 ms LOOP ELSE ?[BUTTONS] THEN
;
: OPERATE
    CLEAR CLS
    BEGIN
      !SP SCAN
      $1B ?= ^Q ?= OR
    UNTIL
;


: !LIFT		_ups 203 ERASE  1 _floor C!  25 _dwells C! DWELL ;

: LIFT		!LIFT OFF CURSOR OPERATE ON CURSOR ;

*END*
