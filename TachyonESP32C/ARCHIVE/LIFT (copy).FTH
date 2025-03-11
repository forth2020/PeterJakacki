ESP32
--- LIFT CHALLENGE    102 lines and 1,286 bytes
\ FORGET *LIFT*

: *LIFT*        PRINT" Simple lift emulator " ;
0 bytes *lift*
: .TITLE        PRINT"   LIFT SIMULATOR - PBJ   "  ;
: .HEAD         PRINT"  * FORTH2020 * " ;
: DING 		7 EMIT ;
: PLAY$		DROP ;
: CRLF			CR ;

{
: EMITS ( ch cnt -- )	1- FOR DUP EMIT NEXT DROP ;
: CLS			^L EMIT ;

: CURSOR ( on/off -- )	ESC[ '?' EMIT 25 PRINT IF 'h' ELSE 'l' THEN EMIT ;
: ?EXIT			IF R> DROP EXIT THEN ;
: 0EXIT			0= IF R> DROP EXIT THEN ;
}

\ 2000 50 TONE 1000 50 TONE 500 100 TONE 250 50 TONE ;

\ ALIAS DO ?DO

{
An exercise in controlling a lift in Forth and how to go about it

A lift has 32 levels including ground floor and basement 1, basement 2

If a lift is going up it needs to know if it has to stop at a floor (dest or up source)
An encoder feeds back the position of the lift including in-between floors

https://tinyurl.com/LiftChallenge

}

64 value #MAX
{
( it would be good to be able to reserve data in a segment but still use byte/long etc )
variable _dseg
: <dseg	_dseg @ org  _dseg +! ;
: dseg> ;
256 dseg
}

( skip 0, use from floor #1 )
#MAX 1+ bytes ups
#MAX 1+ bytes downs
#MAX 1+ bytes gotos

byte _floor
byte _lflags
byte _lift      --- lift status - up/down
byte _dwell     --- dwell timer
byte *dwell     --- dwell multiplier
long num        --- entry number

: .VARS
        CR ups
;


: CLEAR                 num ~ ;
: +NUM ( ch -- )        '0' - num @ 10 * + num ! ;
: NUM@                  num @ CLEAR ;

: DWELL                 6 _dwell C! DING ;
: DWELL?                _dwell C@ ;
: OPEN			;
: CLOSE			;


16 value FLOORS
: FLOOR@                _floor C@ ;
: BOTTOM?               FLOOR@ 1 = ;
: TOP?                  FLOOR@ FLOORS = ;
: lift=                 _lift C@ 7 AND = ;
: GOING-UP?             1 lift= ;
: GOING-DOWN?           2 lift= ;
: IDLE?                 0 lift= ;
: PARK                  0 _lift C! ;
: GO-UP                 1 _lift C! ;
: GO-DOWN               2 _lift C! ;
: DOWN? ( flr -- f )    downs + C@ ;
: UP? ( flr -- f )      ups + C@ ;
: GOTO? ( flr -- f )    gotos + C@ ;
: REQUEST? ( flr -- f ) DUP DOWN? OVER UP? + SWAP GOTO? + ;
: GOTOS? ( -- cnt )     0 FLOORS 1+ 1 DO I GOTO? + LOOP ;
: REQUESTS? ( -- cnt )  0 FLOORS 1+ 1 DO I REQUEST? + LOOP ;
: LIFT? ( flr -- flg )  FLOOR@ = ;
: SPAC                  '_' EMIT ;
: SPACS                 '_' SWAP EMITS ;

: EMOJI         	$F0 EMIT $9F EMIT DUP 6 >> $98 + EMIT $3F AND $80 + EMIT ;
\ $  ITALIC $
: PERSON		$3CD  EMOJI ;
: [PEOPLE]		'[' EMIT PERSON ']' EMIT ;
: .PEOPLE		IF PERSON PERSON THEN ;

\ : .UP			PRINT" UP" ;
: ?EMOJI		SWAP IF EMOJI ELSE DROP 2 SPACS THEN ;
: .UP			$209 ?EMOJI ;
: .DN			$20B ?EMOJI ;
: .GO			$1E0 ?EMOJI ;
: .[UP]			PRINT" [ UP]" ;
: .[DN]			PRINT" [ DN]" ;

: .LIFT \ ( floor -- )
    red PAPER BOLD
    DWELL? IF green PAPER PRINT" _____" EXIT THEN
    GOING-UP? IF GOTOS? IF [PEOPLE] ELSE .[UP] THEN EXIT THEN
    GOING-DOWN? IF GOTOS? IF [PEOPLE] ELSE .[DN] THEN EXIT THEN
    IDLE? IF PRINT" [---]" EXIT THEN
;
: .WALL			PLAIN BOLD '|' EMIT PLAIN ;
: .DOOR ( flr -- )      LIFT? DWELL? AND IF SPAC ELSE .WALL THEN PLAIN ;
: .CABLE		FLOOR@ > IF 5 PEN ."   |  " ELSE 5 SPACES THEN PLAIN ;
: .FLOOR ( floor -- floor )
    CRLF DUP green PAPER 2 U.R PLAIN
    .WALL
    DUP LIFT? IF .LIFT ELSE DUP .CABLE THEN
    DUP .DOOR
    PLAIN blue PAPER 15 SPACS PLAIN .WALL
    $08 16 EMITS
    PLAIN blue PAPER
    DUP UP? .UP
    DUP DOWN? .DN
    DUP GOTO? .GO
    DUP UP? OVER DOWN? OR .PEOPLE
    PLAIN
;
: .WINCH
        CRLF PRINT"    =[" FLOOR@ 1 & IF 'x' ELSE '+' THEN EMIT  PRINT" ]="
        SPACE BLINKING yellow PEN magenta PAPER
        .HEAD PLAIN UL SPACE PLAIN
;
: SHOW
        OFF CURSOR
        HOME  BOLD REVERSE .TITLE PLAIN
        CRLF '=' 25 EMITS
        .WINCH
        FLOORS DUP 0 DO .FLOOR 1- LOOP DROP
        CRLF '=' 25 EMITS
        CRLF num @ ?DUP IF . ELSE 8 SPACES THEN
;
--- UP DOWN buttons on each floor
: UP ( floor -- )               ups + C++ ;
: DOWN ( floor -- )             downs + C++ ;
: GO ( floor -- )               gotos + C++ ;
: STEP
        GOING-UP? IF FLOOR@ FLOORS <> IF _floor C++ SHOW THEN THEN
        GOING-DOWN? IF FLOOR@ 1 <> IF -1 _floor C+! SHOW THEN THEN
;
{
If lift is idle and a button is pressed then the priority is lift button first, then closer floors next
}
: ?FLOOR        FLOOR@ GOTO? IF FLOOR@ gotos + C~ DWELL THEN ;
: ?UPS          GOING-DOWN? 0= FLOOR@ UP? AND IF FLOOR@ ups + C~ DWELL THEN ;
: ?DOWNS        GOING-UP? 0= FLOOR@ DOWN? AND IF FLOOR@ downs + C~ DWELL THEN ;
: ?GO-UP
        GOING-DOWN? ?EXIT
        TOP? ?EXIT
        FLOORS 1+ FLOOR@ 1+ DO I UP? I GOTO? OR IF GO-UP STEP LEAVE THEN LOOP
;
: ?GO-DOWN
        GOING-UP? ?EXIT
        BOTTOM? ?EXIT
        0 FLOOR@ 1- ?DO I DOWN? I GOTO? OR IF GO-DOWN STEP LEAVE THEN -1 +LOOP
;
: ?IDLE
        IDLE? 0EXIT
         FLOORS 1+ FLOOR@ 1+ ?DO I DOWN? IF GO-UP STEP LEAVE THEN LOOP
         IDLE? 0EXIT
         BOTTOM? ?EXIT

         0 FLOOR@ 1- ?DO I UP? IF GO-DOWN STEP LEAVE THEN -1 +LOOP
;
: COMPUTE
        REQUESTS? 0EXIT
        ?FLOOR ?UPS ?DOWNS
	_dwell C@ 0= IF PARK ?GO-UP ?GO-DOWN ?IDLE THEN
;


                ( CONSOLE BUTTONS )
{
This "Check Buttons" routine gets called every 100ms within the dwell delay loop
It will capture serial console keys and process digits or single commands.
Pressing 1 2 3 will simply builds 123 which can be read by NUM@
1U is the same as typing 1 UP to request an up lift from floor 1
6G is the same as typing 6 GO to go to the 6th floor when in the lift
}

: ?BUTTONS
    KEY? IF KEY
      DUP 'a' 'z' >< IF $20 - THEN				--- convert from lower-case
      ?!							--- set case test variable
      '0' '9' ?>< IF ?@ +NUM THEN                   		--- process digits into NUM
      'U' ?= iF NUM@ UP SHOW THEN				--- request UP from NUM floor
      'D' ?= IF NUM@ DOWN SHOW THEN				--- request DOWN
      '*' ?= IF NUM@ *dwell C! THEN				--- set new dwell time (speed)
      '#' ?= IF NUM@ #MAX MIN to FLOORS CLS SHOW THEN		--- set new floor size
      'S' ?= IF 20 *dwell C! THEN				--- SLOW down the speed
      'F' ?= IF 1 *dwell C! THEN				--- FAST
      'G' ?=							--- GO to num floor (lift button)
      $0D ?= OR IF NUM@ GO SHOW THEN
      $08 ?= IF CLEAR CLS SHOW THEN				--- clear NUM entry
      $20 ?= IF COMPUTE SHOW THEN				--- single step
    THEN
;

: OP
      !SP SHOW
---    to dwell - or not to dwell or pause or just compute
       DWELL? IF _dwell C-- ELSE $20 ?= 0= IF COMPUTE THEN THEN
---   just check buttons while dwelling
      *dwell C@ 0 DO ?BUTTONS 20 ms LOOP
;
: OPERATE
    CLEAR CLS
    BEGIN
      OP $1B ?= ^Q ?= OR
    UNTIL
;

: !LIFT		ups 204 ERASE  1 _floor C!  25 *dwell C! DWELL ;
: LIFT		!LIFT CLS SHOW " DING" PLAY$ ( 2000 FOR 1 ms KEY DROP NEXT " NONE" PLAY$ ) OPERATE ;

*END*
