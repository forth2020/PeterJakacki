!!! Testing only - use selected copy&paste


: .adc  begin cr 0 adc 50 / ?dup if '*' swap emits then 1 ms key? until ;


FORGET *SCOPE*
: *SCOPE* 	." VERY VERY SIMPLE ESP32-C3 SCOPE ";
: us		3 / ?DUP IF 0 DO LOOP THEN ;
: FIT		4096 _ymax C@ / / ;
: PLOT		_ymax C@ SWAP - XY '*' EMIT ;
: TRIG		BEGIN 0 ADC 2000 2096 >< UNTIL ;
: HOLDOFF	300 ms ;
: SCOPE ( ch us -- )
  OFF CURSOR yellow PEN
  BEGIN
     TRIG
     CLS plain 1 _ymax C@ 2/ XY   '-' _xmax C@ EMITS
     yellow bold pen
    _xmax C@ 1+ 1 DO OVER ADC I SWAP FIT PLOT DUP us LOOP
    HOLDOFF
    KEY?
  UNTIL
  KEY DROP 2DROP ON CURSOR
;






: fibo ( n -- f )  		0 1 ROT 1- FOR BOUNDS NEXT DROP ;
: fibos  1 46 BOUNDS DO CR I ." fibo(" I . ." ) = " LAP I fibo  LAP .LAP ."  result = " . 5 +LOOP ;

: 2OVER   >R >R 2DUP R> -ROT R> -ROT ;
: 2SWAP   >R -ROT R> -ROT ;
\ : D+      ROT + >R 2DUP + ;
\ al bl bh+ah

0. 1. n-1 for

: fibo ( n -- f. )  		0 1 ROT 1- FOR BOUNDS NEXT DROP ;
: fibos  1 46 BOUNDS DO CR I ." fibo(" I . ." ) = " LAP I fibo  LAP .LAP ."  result = " . 5 +LOOP ;



: .stats

	xy@ >A >A home .s
;




: DINGBAT       $E2 EMIT DUP 6 >> $9C + EMIT $3F AND $80 + EMIT ;

--- $100..1FF DINGBATS are octels (vs sixtels)
: EMS     bounds DO I 15 AND 0= IF cr I .W space THEN I EMOJI LOOP ;

: DINGS     bounds DO I 15 AND 0= IF cr I .W space THEN I DINGBAT LOOP ;

: DUCK		$386 EMOJI ;

[[ $200 $100 CR DO I DINGBAT 3 ms 0 emit LOOP ]]


0000 😀😁😂😃😄😅😆😇😈😉😊😋😌😍😎😏
0010 😐😑😒😓😔😕😖😗😘😙😚😛😜😝😞😟
0020 😠😡😢😣😤😥😦😧😨😩😪😫😬😭😮😯
0030 😰😱😲😳😴😵😶😷😸😹😺😻�                      �
�😽😾😿
0040 🙀🙁🙂🙃🙄🙅🙆🙇🙈🙉🙊🙋�                      �
�🙍🙎🙏
01E0 🟠🟡🟢🟣🟤🟥🟦🟧🟨🟩🟪🟫�
0300 🤀🤁🤂🤃🤄🤅🤆🤇🤈🤉🤊🤋�                                  �
�🤍🤎🤏
0310 🤐🤑🤒🤓🤔🤕🤖🤗🤘🤙🤚��                       �
🤞🤟
0320 🤠🤡🤢🤣🤤🤥🤦🤧🤨🤩🤪🤫�                      �
�🤭🤮🤯
0330 🤰🤱🤲🤳🤴🤵🤶🤷🤸🤹🤺🤻🤼�                     �
��🤾🤿
0340 🥀🥁🥂🥃🥄🥅🥆🥇🥈🥉🥊🥋�                       �
�🥍🥎🥏
0350 🥐🥑🥒🥓🥔🥕🥖🥗🥘🥙🥚🥛�                      �
�🥝🥞🥟
0360 🥠🥡🥢🥣🥤🥥🥦🥧🥨🥩🥪🥫�                      �
�🥭🥮🥯
0370 🥰🥱🥲🥳🥴🥵🥶🥷🥸🥹🥺🥻�                      �
�🥽🥾🥿
0380 🦀🦁🦂🦃🦄🦅🦆🦇🦈🦉🦊🦋�                      �
�🦍🦎🦏
0390 🦐🦑🦒🦓🦔🦕🦖🦗🦘🦙🦚🦟���                    �
🦞🦟
03A0 🦠🦡🦢🦣🦤🦥🦦🦧🦨🦩🦪🦫�                      �
�🦭🦮🦯
03B0 🦰🦱🦲🦳🦴🦵🦶🦷🦸🦹🦺🦻�                      �
�🦽🦾🦿
03C0 🧀🧁🧂🧃🧄🧅🧆🧇🧈🧉🧊🧋�                      �
�🧍🧎🧏
03D0 🧐🧑🧒🧓🧔🧕🧖🧗🧘🧙🧚🧟���                    �
🧞🧟
03E0 🧠🧡🧢🧣🧤🧥🧦🧧🧨🧩🧪🧫�                      �
�🧭🧮🧯
03F0 🧰🧱🧲🧳🧴🧵🧶🧷🧸🧹🧺🧻�                      �
�🧽🧾🧿
04E0 🫠🫡🫢🫣🫤🫥🫦🫧🫨
04F0 🫰🫱🫲🫳🫴🫵🫶🫷🫸

$500 SIXTELs




variable @A 15 cells allot
@A CELL+	constant @B
@B CELL+	constant @C
@C CELL+	constant @D

: A!		@A ! ;
: A		@A @ ;
: B		@B @ ;
: C		@C @ ;
: D		@D @ ;
: >A		@A @B 60 CMOVE> A! ;
: DROPA		@B @A 60 CMOVE ;
: A> 		A DROPA ;

( so rather than use the return stack for temps - just push and pop multiples using aux stack )
: PUSHA ( <n> cnt -- ) 	1- FOR >A NEXT ;
: POPA ( cnt -- <n> )	1- FOR A> NEXT ;
( AUX STACK EXTRAS )
: A@+		A @ CELL @A +! ;
: AW@+ ( -- w )	A UW@ 2 @A +! ;
: AC@+ ( -- b )	A C@ @A ++ ;





{
{
: aprompt    		plain blue pen  >r >r depth str type r> r> 35 emit 62 emit space plain  ;
' aprompt value _prompt
: !prompt   		['] aprompt to _prompt ;
: prompt     		_prompt execute ;
}

( COMPEX )

: TIBCH ( -- ch )	>in @ C@ >in ++ ;
: TIB$ ( -- a n )
	begin tibch dup $20 = while drop repeat
	>in c@ swap if begin tibch $20 <> until >in c@ over - else 0 then
	;

: ?NUMBER
: COMPEX
	BEGIN TIB$ ?DUP WHILE ?NUMBER  REPEAT
;

( REPL )
: eolok   	    	plain blue pen ."  ok" cr ;
' eolok value _eol
: eol 		    	_eol execute ;


: evaluate-buffer   	begin >in @ #tib @ < while evaluate1 ?stack repeat ;

: evaluate ( a n -- ) 	'tib @ >r #tib @ >r >in @ >r
                     	#tib ! 'tib ! >in ~ evaluate-buffer
                      	r> >in ! r> #tib ! r> 'tib ! ;
: evaluate&fill
   begin >in @ #tib @ >= if eol refill drop then evaluate-buffer again ;
: quit
   #tib @ >in !
   begin ['] evaluate&fill catch
         if state ~  fp0 fp! rp0 rp! .ERROR cr then
	 depth 0< if !sp then
   again ;
}





( upper case vs lower case )

ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz ok

    $1B5B42 ?= ( DN )	IF hist# C@ IF hist# C-- hist# C@ IF GETHIST EXIT THEN THEN CLRTIB REPROMPT THEN
    $1B5B42 ?= ( DN )	if hist# C@ if hist# C-- hist# C@ if gethist exit then then clrtib reprompt then


	( COMPEX )

long _there : THERE _there @ ;
: [[ HERE _there ! ['] DUMP @ , ['] ] CALL ;
: ]] ['] [ CALL THERE CALL THERE HERE - ALLOT ; immediate

--- simple test of compex mode - final will not need [[ ]] operations
[[ $7F 20 DO I EMIT LOOP EXIT ]]






: Q	10 0 DO: I: . LOOP: ;
0#> ' Q 12 DUMPC ---
3FCAD1B4:  42003E46	DOCOL
3FCAD1B8:  3C0C151C	DOLIT
3FCAD1BC:  0000000A
3FCAD1C0:  3C0C151C	DOLIT
3FCAD1C4:  00000000
3FCAD1C8:  3C0C148C
3FCAD1CC:  3C0C14A4
3FCAD1D0:  3FCA2A0C
3FCAD1D4:  3C0C1498
3FCAD1D8:  3C0C1630
3FCAD1DC:  3FCA1BFC
3FCAD1E0:  3FCAD1B4 ok
0#> ' LOOP: .L --- 3C0C1498 ok
0#>



{
( Examine Memory )

--- locals version
: dump ( A:a B:n -- )
   0 >A >A >A
   begin
     C $0F &! if A .addr then
     C 3 &! if space then
     AC@+ .B SPACE
     @C ++ @B --
     C $0F &! if 4 spaces A A 16 - do I C@ aemit loop then
     B 0=
   until
;


: dump ( a n -- )
   over 15 and if over .addr then
   for aft  ( addr )
     dup 15 and 0= if dup .addr then
     dup 3 and 0= if space then
     dup c@ .B space 1+
     dup 15 and 0= if 4 spaces dup dup 16 - do i c@ aemit loop then
   then next drop cr ;

( quick dump of 32 bytes )
: qd ( addr -- )    $20 dump ;


: dumpa
   over 63 and if over .addr then
   for aft  ( addr )
     dup 63 and 0= if dup .addr then
     dup c@ aemit 1+
   then next drop cr ;
;

: dumpz ( a n )
  8 rshift ( pages )
  0 do
  0 over 256 bounds do i @ or 4 +loop
    if dup 256 dump then 256 +
  loop drop
;

( dump longs )
: dumpl ( addr -- )
   over 31 and if over .addr then
   2/ 2/ for aft
     dup 31 and 0= if dup .addr then
     dup @ .l space 4 +
   then next drop cr ;
}


{

( CSIKEY > ASCII 8-bit msb set

: CSI>ASC8


;

	ASC8	OPEN		SHIFT		CTRL		ALT		CTLSHF		CTLALT
UP	U	$001B5B41 A	$5B313241
DOWN	D	$001B5B42 B	$5B313242
RIGHT	R	$001B5B43 C	$5B313243
LEFT	L	$001B5B44 D	$5B313244	$5B313544	$5B313344	$5B313644	$5B313744

INS	I	$001B5B40 @
DEL	7F	$00001B7F 7F
END	E	$001B4F46 F
HOME	H	$001B5B48 H
PGDN	N	$001B5B55 U
PGUP	P	$001B5B56 V


    $1B5B41 ?= ( UP )	if =UP then
    $1B5B42 ?= ( DN )	if =DN then
    $1B5B43 ?= ( R )	if =R then
    $1B5B44 ?= ( L )	if =L then
    $1B5B40 ?= ( INS )	if =INS then



CB 82	˂	Modifier Letter Left Arrowhead
707	U+02C3	CB 83	˃	Modifier Letter Right Arrowhead
708	U+02C4	CB 84	˄	Modifier Letter Up Arrowhead
709	U+02C5	CB 85	˅	Modifier Letter Down Arrowhead

}






( EXTEND: NEW ACCEPT WITH HISTORY )
: syspen		plain blue pen ;
: usrpen		plain bold yellow pen space 8 emit ;
: uprompt    		$0d emit syspen depth str type '#' emit '>' emit space plain  ;
internals
input-limit bytes historys  ( single level for testing )
: history>		historys input-buffer input-limit cmove ;
: >history		input-buffer historys input-limit cmove ;
long adr long mx long cnt : cnt@ cnt @ ;
: =CR			plain blue pen ."  --- "  plain ;
: =UP			history> input-buffer len$ cnt ! historys cnt@ type ;
: =BS			cnt@ if adr -- cnt -- 8 ?echo bl ?echo 8 ?echo then ;
	( key handlers )
: ?keys
    ?!
    ?@ $20 $7E >< cnt@ mx @ < & if ?@ dup ?echo adr @ w! adr ++ cnt ++ exit then
    ^M ?= ( CR )	if =CR >history exit then
    ^H ?= ( BS )	if =BS then
    ^Q ?= ( .S ) 	if .s then
    ^S ?= ( !SP )	if !sp then
    ^W ?= ( WORDS )	if cwords then
    ^L ?= $1B5B48 ?= |	if ^L EMIT .BOOT !sp then
    ^Z ?= ( ZAP )	if 0 CALL then
    ^? ?= ( QUERY )	if ok then
    ^X ?= ( execute )	if =UP =CR $0D ?! then
    ^[ ?= ( esc )	if uprompt 80 spaces uprompt adr @ cnt@ - adr ! cnt ~ then
    $1B5B41 ?=		if =UP then
;
forth
: accept2 ( adr max -- cnt )
   mx ! adr ! cnt ~
   usrpen begin csikey ?keys ?@ $0d = until
   cnt@
;
' accept2 to _accept ( redirect accept )


{
  \ NOTE: to be rebuilt in EXTEND perhaps
( REPL )
: eolok   	    	plain blue pen ."  ok" cr ;
' eolok value _eol
: eol 		    	_eol execute ;
: evaluate-buffer   	begin >in @ #tib @ < while evaluate1 ?stack repeat ;
: evaluate ( a n -- ) 	'tib @ >r #tib @ >r >in @ >r
                     	#tib ! 'tib ! >in ~ evaluate-buffer
                      	r> >in ! r> #tib ! r> 'tib ! ;
: evaluate&fill
   begin >in @ #tib @ >= if eol refill drop then evaluate-buffer again ;
: quit
   #tib @ >in !
   begin ['] evaluate&fill catch
         if state ~  fp0 fp! rp0 rp! .ERROR cr then
	 depth 0< if !sp then
   again ;
}







( NEW ACCEPT WITH HISTORY )

variable adr : adr@ adr @ ; : adr! adr ! ;
variable cnt : cnt@ cnt @ ; : cnt! cnt ! ;
variable lim : max@ lim @ ; : max! lim ! ;
: c@+ 			dup 1+ swap c@ ;
: len$ ( adr -- cnt )	dup begin c@+ 0= until swap - ;

200 constant input-limit
: tib ( -- a ) 		'tib @ ;
create input-buffer   	input-limit allot
: tib-setup   		input-buffer 'tib ! ;

create history-buffer	input-limit allot
: history>		history-buffer input-buffer input-limit cmove ;
: >history		input-buffer history-buffer input-limit cmove ;

: refill   		tib-setup tib input-limit prompt accept #tib ! >in ~ -1 ;

: eat-till-cr   	begin *key dup 13 = if ?echo exit else drop then again ;

: ?CR			dup 13 = if plain blue pen ."  --- "  plain >history r> drop cnt@ then ;
: BS			cnt@ if adr -- cnt -- 8 ?echo bl ?echo 8 ?echo then ;
: >TIB ( key -- )	dup ?echo adr@ w! adr ++ cnt ++ ;
: ?CHAR			dup 8 = over 127 = or if BS else >TIB then ;
: ?UP			dup 21 = if drop history> input-buffer len$ cnt! 13 then ;
: accept2 ( adr max -- cnt )
   max! adr! 0 cnt!
   plain bold yellow pen space 8 emit
   begin 	adr@ max@ <
   while 	*key ?UP ?CR ?CHAR
   repeat
   eat-till-cr plain cnt@
;

' accept2 to _accept


( ORIGINAL )


: eat-till-cr   begin *key dup 13 = if ?echo exit else drop then again ;
: accept ( adr max -- cnt )
   0 swap plain bold yellow pen space 8 emit
   begin 2dup <
   while ( adr cnt max )
     *key ( adr cnt max key )
     dup 13 = if plain blue pen ."  --- " 2drop nip plain exit then
     dup 8 = over 127 = or
     if drop over if rot 1- rot 1- rot 8 ?echo bl ?echo 8 ?echo then
     else
       dup ?echo
       >r rot r> over c! 1+ -rot swap 1+ swap
     then
   repeat drop nip
   eat-till-cr plain
;
' accept1 value _accept
: accept		_accept execute ;

200 constant input-limit
: tib ( -- a ) 'tib @ ;
create input-buffer   	input-limit allot
: tib-setup   		input-buffer 'tib ! ;
: refill   		tib-setup tib input-limit prompt accept #tib ! >in ~ -1 ;

















$3fca0b78
'tib	pointer to tib
#tib	tib count
>in







\ : LAP			$44 @STR @ ;

: LAP			$60023044 @ ;
: .LAP			- ABS 4 rshift . ." us" ;

: HIGH 	;
(                               800us            1000us  2)
: Q1	2 output pinmode begin 2 0 digitalwrite 2 1 digitalwrite again ;


0 	value _ansi
: ANSI			_ansi 0= IF FOR AFT DROP THEN NEXT R> THEN DROP ;


: ms@			ms-ticks ;

--- timeout if no key pressed
: ?KEY ( ms -- key|0 )	ms@ + BEGIN KEY? 0= WHILE DUP ms@ = IF DROP 0 EXIT THEN REPEAT DROP KEY ;
--- testing ANSI checker
--- wait for key input and getnum - terminate on timeout or invalid key
: ?NUM ( ms -- n )
    0 BEGIN OVER ?KEY DUP ` 0 ` 9 ><
    WHILE $30 - SWAP 10 * +
    REPEAT DROP NIP
;

: XY? ( -- x y )
    ESC[ ` 6 EMIT ` n EMIT
    20 ?KEY $1B = IF 20 ?KEY DROP 20 ?NUM 20 ?NUM SWAP ELSE 0 0 THEN
;
: VT? ( -- xmax ymax )
    XY? 999 999 XY XY? >R >R XY R> R>
    ;






{ Testing high level do loop stack
0#> : Q1 0 DO: LOOP: ;
0#> LAP 1000000 Q1 LAP .LAP
1648 ms ok

1#> : Q2 0 DO LOOP ;
1#> LAP 1000000 Q2 LAP .LAP
2962 ms ok

result is almost twice as fast
}
{

0#> : q1 1,000,000 0 lap do: loop: lap .lap ; ---  ok
0#> q1 --- 138 ms ok
0#> : q2 1,000,000 0 lap do: 1234 dup + drop loop: lap .lap ; ---  ok
0#> q2 --- 597 ms ok
0#>

-> 1234 dup + drop = 459ns

}


variable acpt

: accept ( a max -- cnt )
   swap acpt ! 0 ( max cnt )
   begin 2dup >
   while ( max cnt )
     *key
     dup 13 = if ?echo nip exit then
     dup 8 = over 127 = or
       if drop dup if -1 acpt +! 1-
         8 ?echo bl ?echo 8 ?echo
       then
     else
       dup ?echo acpt @ c!  acpt ++ 1+
     then
   repeat nip
   eat-till-cr
;




{
( testing simple and fast do loop stack - 4 levels )
variable dostack -4 allot
variable index	variable limit	variable branch
variable dostacks 8 cells allot

: DO:  		dostack dostacks 9 cells cmove 	R@ branch ! index ! limit ! ;
: I:		index @ ;
: J:		dostacks @ ;
: LEAVE:	limit 1- index ! ;
: UNLOOP:	dostacks dostack 9 cells cmove ;
: LOOP:   	index ++ I: limit @ <> IF branch @ R! ELSE UNLOOP: THEN ;

: TEST-DO:	0 DO: LOOP: ;
: TEST-DO	0 DO LOOP ;
}
{
Testing loop stack speed - at high-level
0#> 1000000 LAP TEST-DO LAP .LAP
2962 ms ok
0#> 1000000 LAP TEST-DO: LAP .LAP
1647 ms ok

Result: Loop stack is almost twice as fast
Note: kernel code version will be very fast.

X("DO:", DOO, branch = *(uint32_t *) ip; index = tos; *(uint32_t *) limit = *sp--; NIP; DROP) \
 X("LOOP:", LOOPY,  if (++index != limit) (uint32_t) ip = branch; else ) \

}





{
edit: function keys still need to be considered??? - Maybe 2 bytes?
CSI compressed code to single standard 8-bit character
CSI CODE: %1_shift_ctrl_alt_key_key_key_key
csitbl holds 128 longs corresponding to the CSI code
The cell index (+1) of that code is the result code plus msb set
Using the index keeps the alignment of the 32-bit cell simple
rather than trying to add in the 8-bit result.
}

variable csitbl -4 allot
$001B5B44 ,	\ $81 LEFT
$001B5B43 ,	\ $82 RIGHT
$001B5B41 ,	\ $83 UP
$001B5B42 ,	\ $84 DOWN
$1B5B317E ,	\ $85 HOME
$001B4F46 ,	\ $86 END
$1B5B357E ,	\ $87 PGUP
$1B5B367E ,	\ $88 PGDN
$5B33327E ,	\ $89 DELETE


$001B4F50 ,	\ $ F1
$001B4F51 ,	\ $ F2
$001B4F52 ,	\ $ F3
$001B4F53 ,	\ $ F4
$5B31367E ,	\ $ F5
$5B31377E ,	\ $ F6
$5B31387E ,	\ $ F7
$5B31397E ,	\ $ F8
$5B32307E ,	\ $ F9
$5B32317E ,	\ $ F10
$00000000 ,	\ $ F11 system use
$001B4F59 ,	\ $ F12

{
    $001B4F50 , ' F1 ,          $001B4F51 , ' F2 ,
    $001B4F52 , ' F3 ,          $001B4F53 , ' F4 ,
    $001B4F54 , ' F5 ,          $001B4F55 , ' F6 ,
    $001B4F56 , ' F7 ,          $001B4F57 , ' F8 ,
    $001B4F58 , ' F9 ,          $5B32317E , ' F10 ,
    $001B4F59 , ' F12 ,
}
$5B313244 , 	\ SHIFT LEFT
$5B313243 ,	\ SHIFT RIGHT
$5B313241 ,	\ SHIFT UP
$5B313242 ,	\ SHIFT DOWN


{
0#> : DEMO BEGIN CR CSIKEY DUP $1B <> WHILE .L REPEAT ;
0#> DEMO
00000031	\ 1
00000032	\ 2
001B5B44	\ LEFT
001B5B43	\ RIGHT
001B5B41	\ UP
001B5B42	\ DOWN
1B5B317E	\ HOME
001B4F46	\ END
1B5B357E	\ PGUP
1B5B367E	\ PGDN

CSI->8-bits idea
LEFT RIGHT UP DOWN HOME END PGUP PGDN DELETE INSERT --> 4-bit code b3..b0
SHIFT CTRL ALT --> 3-bit code b6..b4
CSI CODE: %1_shift_ctrl_alt_key_key_key_key
MSB set
For example:
$81 = LEFT
$C1 = SHIFT LEFT
$A1 = CTRL LEFT
$91 = ALT LEFT
$F1 = SHIFT CTRL ALT LEFT
}


{
: SUB   I: . ;
: MAIN  10 0 DO: SUB LOOP: ;
}
{
0#> : HELLOTHERE ;
0#> $3FCAA430 $20 DUMP
3FCA_A430:   48 45 4C 4C  4F 54 48 45  52 45 AA D6  1C A4 CA 3F     HELLOTHERE.....?
3FCA_A440:   00 0A 01 00  38 3D 00 42  B4 10 0C 3C  4C C4 C3 D0     ....8=.B...<L...
0#> ' HELLOTHERE .L
3FCAA444 ok
0#> HERE .L
3FCAA44C ok
}
: CSI>CHAR ( csi -- char )

;



{
!"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~
nOT PRESENT in CSI seq $5C..$7D = 34 codes + $00..$2F = 47 codes = 81 codes
\]^_`abcdefghijklmnopqrstuvwxyz{|}
PRESENT
0..9 : @ A..Z  ESC [ ~ DEL = 42 codes -> 6 bits
or $30..$5B $7E,$7F
or 0..9 : @ A..Z ~ DEL = 40 codes -> modulo 40 in 32-bits = 6 characters.
}
\   123456789_123456789_123456789_123456789_
\ z" 0123456789:@ABCDEFGHIJKLMNOPQRSTUVWXYZ[~"

\ : LEN$ ( zstr -- len )  100 0 DO DUP I + C@ 0= IF DROP I LEAVE THEN LOOP DUP $200 > IF DROP 0 THEN ;



: CSIKEY2 ( --- code )
    KEY DUP $1B =
    IF  ms-ticks SWAP
      BEGIN KEY?
\                ignore ;      else merge to code       update timeout
        IF KEY DUP $2F > OVER $5B < AND OVER $7D > OR
	  IF $30 - ( 00..$2B ) DUP $40 > IF $22 - ( $7F->4F->2D ) THEN \ 46 codes
	  ELSE DROP
	  THEN


	 DUP [char] ; = IF DROP ELSE SWAP 8 << OR THEN   NIP ms-ticks SWAP
	THEN
\     timeout after 3ms of inactivity - must be end of sequence
        OVER ms-ticks - ABS 5 >
      UNTIL NIP
    THEN ;


\ tool to collect CSI codes while allowing text
: CSIKEYS 		BEGIN CSIKEY DUP $1B <> WHILE DUP $7F > IF CR .L SPACE ELSE EMIT THEN REPEAT ;

CSI CODES ANSI MODE
001B5B44 LEFT
001B5B43 RIGHT
001B5B41 UP
001B5B42 DOWN
001B5B48 HOME
001B4F46 END
001B5B56 PGUP
001B5B55 PGDN
00001B7F DEL
001B5B40 INS
001B4F50 F1
001B4F51 F2
001B4F52 F3
001B4F53 F4
001B4F54 F5
001B4F55 F6
001B4F56 F7
001B4F57 F8
001B4F58 F9

3231357E F10 C
3233357E F11 C
3234357E F12 C
001B4F59 F12

5B313250 F1 S
5B313251 F2 S
5B313252 F3 S
5B313253 F4 S
3135327E F5 S
3137327E F6 S
3138327E F7 S
3139327E F8 S
3230327E F9 S
3233327E F11 S
3234327E F12 S

5B313244 LEFT S
5B313243 RIGHT S
5B313241 UP S
5B313242 DOWN S



  X("I:", DOQ, DUP; tos = index) \
3C0C_0FC4: I:
3C0C0FC4:
3C76 	c.jal	001
4200
1FEC
3C0C
0708
0000
3C94
4200


 X("LOOP:", LOOPY,  if (++index != limit) ip = (cell_t *) branch; else ) \
3C0C_0FB8: LOOP:
42003C54
3C0C1FE8
00000208 42003C76 3C0C1FEC 00000708 42003C94 3C0C1FF4
 ok


: MEM? ( zs from -- a )  swap @ >A BEGIN DUP @ A@ <> WHILE 1+ REPEAT QD ;


08 02 00 00  76 3C 00 42  EC 1F 0C 3C0C1FEC42003C7600000208



internals
: cwords
    0 TO line-pos
    context @ @
    BEGIN  DUP WHILE  ?ins. DUP onlines see. >link REPEAT
    DROP cr
;



: .COLOR	plain DUP 1 = IF bold cyan pen DROP ELSE IF magenta pen THEN THEN ;
internals
: .words
    0 TO line-pos
    context @ @
    BEGIN  DUP WHILE CR plain DUP .L SPACE DUP CELL - C@ .COLOR SPACE DUP see. >link REPEAT
    DROP cr
;
: cwords
    0 TO line-pos
    context @ @
    BEGIN  DUP WHILE plain ?ins. DUP CELL - C@ .COLOR SPACE DUP onlines see. >link REPEAT
    DROP cr
;



( Erase the screen from the current location )
: ERSCN			ESC[ [char] 2 EMIT [char] J EMIT ;
( Erase the current line )
: ERLINE		ESC[ [char] 2 EMIT [char] K EMIT ;
: asw			IF [char] h ELSE [char] l THEN EMIT ;
: CURSOR ( on/off -- )	ESC[ [char] ? EMIT 25 PRINT asw ;
: WRAP ( on/off -- )	ESC[ [char] ? EMIT [char] 7 EMIT asw ;








--- wait for key input and getnum - terminate on timeout or invalid key
pri GETNUM ( ms -- n )
    0 BEGIN OVER ?KEY DUP '0' '9' WITHIN
    WHILE $30 - SWAP 10 * +
    REPEAT DROP NIP
;

pub %XY@ ( -- x y )
    '6' ESCB 'n' EMIT
    20 ?KEY $1B = IF 20 ?KEY DROP 20 GETNUM 20 GETNUM SWAP ELSE 0 0 THEN
;
pub VT? ( -- xmax ymax )
    %XY@ 999 999 %XY %XY@ 2SWAP %XY
    ;















	( ANSI CSI TERMINAL SUPPORT )

: ESC ( ch -- )		$1B EMIT EMIT ;
: CSI ( ch -- )		` [ ESC EMIT ;
: HOME			` H CSI ;
: COL ( col fg/bg -- )	CSI ` 0 + EMIT ` m EMIT ;
: PEN ( col -- )	7 AND ` 3 COL ;
: PAPER ( col -- )	` 4 COL ;
: .PAR			SWAP PRINT EMIT ;
: CUR ( cmd n -- )	` [ ESC SWAP .PAR ;
: XY ( x y -- )		` ; SWAP CUR ` H .PAR ;
: CLS 			$0C EMIT ;
--- Erase the screen from the current location
: ERSCN			` 2 CSI ` J EMIT ;
--- Erase the current line
: ERLINE		` 2 CSI ` K EMIT ;
: asw			IF ` h ELSE ` l THEN EMIT ;
: CURSOR ( on/off -- )	` ? CSI 25 PRINT asw ;
: WRAP ( on/off -- )	` ? CSI ` 7 EMIT asw ;

--- 0 plain 1 bold 2 dim 3 rev 4 uline
: ATR ( ch -- )		CSI ` m EMIT ;
: PLAIN			` 0 ATR ;
: REVERSE		` 7 ATR ;
: BOLD			` 1 ATR ;
: UL          		` 4 ATR ;
: BLINKING	        ` 5 ATR ;


--- ANSI COLORS
0	constant black
1	constant red
2	constant green
3	constant yellow
4	constant blue
5	constant magenta
6	constant cyan
7	constant white










( CONFIG )
GPIO_BT_SELECT_REGGPIO bit select register0x0000R/W
GPIO_OUT_REGGPIO output register0x0004R/W/SS
GPIO_OUT_W1TS_REGGPIO output set register0x0008WT
GPIO_OUT_W1TC_REGGPIO output clear register0x000CWT
GPIO_ENABLE_REGGPIO output enable register0x0020R/W/SS
GPIO_ENABLE_W1TS_REGGPIO output enable set register0x0024WT
GPIO_ENABLE_W1TC_REGGPIO output enable clear register0x0028WT
GPIO_STRAP_REGpin strapping register0x0038RO
GPIO_IN_REGGPIO input register0x003CRO
GPIO_STATUS_REGGPIO interrupt status register0x0044R/W/SS
GPIO_STATUS_W1TS_REGGPIO interrupt status set register0x0048WT
GPIO_STATUS_W1TC_REGGPIO interrupt status clear register0x004CWT
GPIO_PCPU_INT_REGGPIO PRO_CPU interrupt status register0x005CRO
GPIO_PCPU_NMI_INT_REGGPIO PRO_CPU (non-maskable) interrupt status0x0060RO
GPIO_STATUS_NEXT_REG interrupt source register0x014CRO

( PIN CONFIG )
$0074			GPIO_PIN0_REG			--- GPIO pin0 configuration register  R/W
$0078			GPIO_PIN1_REG			--- GPIO pin1 configuration register  R/W
$007C			GPIO_PIN2_REG			--- GPIO pin2 configuration register  R/W
$0080			GPIO_PIN3_REG			--- GPIO pin3 configuration register  R/W
$0084			GPIO_PIN4_REG			--- GPIO pin4 configuration register  R/W
$0088			GPIO_PIN5_REG			--- GPIO pin5 configuration register  R/W
$008C			GPIO_PIN6_REG			--- GPIO pin6 configuration register  R/W
$0090			GPIO_PIN7_REG			--- GPIO pin7 configuration register  R/W
$0094			GPIO_PIN8_REG			--- GPIO pin8 configuration register  R/W
$0098			GPIO_PIN9_REG			--- GPIO pin9 configuration register  R/W
$009C			GPIO_PIN10_REG			--- GPIO pin10 configuration register R/W
$00A0			GPIO_PIN11_REG			--- GPIO pin11 configuration register R/W
$00A4			GPIO_PIN12_REG			--- GPIO pin12 configuration register R/W
$00A8			GPIO_PIN13_REG			--- GPIO pin13 configuration register R/W
$00AC			GPIO_PIN14_REG			--- GPIO pin14 configuration register R/W
$00B0			GPIO_PIN15_REG			--- GPIO pin15 configuration register R/W
$00B4			GPIO_PIN16_REG			--- GPIO pin16 configuration register R/W
$00B8			GPIO_PIN17_REG			--- GPIO pin17 configuration register R/W
$00BC			GPIO_PIN18_REG			--- GPIO pin18 configuration register R/W
$00C0			GPIO_PIN19_REG			--- GPIO pin19 configuration register R/W
$00C4			GPIO_PIN20_REG			--- GPIO pin20 configuration register R/W
$00C8			GPIO_PIN21_REG			--- GPIO pin21 configuration register R/W

( INPUT FUNCTION )
$0158			GPIO_FUNC1_IN_SEL_CFG_REG	--- Configuration register for input signal 1 R/W

$034C 			GPIO_FUNC126_IN_SEL_CFG_REG	--- Configuration register for input signal 126 R/W
$0350			GPIO_FUNC127_IN_SEL_CFG_REG	--- Configuration register for input signal 127 R/W

( OUTPUT FUNCTION )
: OUTFNC ( n -- addr )	2* 2* $0554 + GPIO + ;
$0554			GPIO_FUNC0_OUT_SEL_CFG_REG	--- Configuration register for GPIO0 output  R/W
$0558			GPIO_FUNC1_OUT_SEL_CFG_REG	--- Configuration register for GPIO1 output  R/W
$055C			GPIO_FUNC2_OUT_SEL_CFG_REG	--- Configuration register for GPIO2 output  R/W
$0560			GPIO_FUNC3_OUT_SEL_CFG_REG	--- Configuration register for GPIO3 output  R/W
$0564			GPIO_FUNC4_OUT_SEL_CFG_REG	--- Configuration register for GPIO4 output  R/W
$0568			GPIO_FUNC5_OUT_SEL_CFG_REG	--- Configuration register for GPIO5 output  R/W
$056C			GPIO_FUNC6_OUT_SEL_CFG_REG	--- Configuration register for GPIO6 output  R/W
$0570			GPIO_FUNC7_OUT_SEL_CFG_REG	--- Configuration register for GPIO7 output  R/W
$0574			GPIO_FUNC8_OUT_SEL_CFG_REG	--- Configuration register for GPIO8 output  R/W
$0578			GPIO_FUNC9_OUT_SEL_CFG_REG	--- Configuration register for GPIO9 output  R/W
$057C			GPIO_FUNC10_OUT_SEL_CFG_REG	--- Configuration register for GPIO10 output R/W
$0580			GPIO_FUNC11_OUT_SEL_CFG_REG	--- Configuration register for GPIO11 output R/W
$0584			GPIO_FUNC12_OUT_SEL_CFG_REG	--- Configuration register for GPIO12 output R/W
$0588			GPIO_FUNC13_OUT_SEL_CFG_REG	--- Configuration register for GPIO13 output R/W
$058C			GPIO_FUNC14_OUT_SEL_CFG_REG	--- Configuration register for GPIO14 output R/W
$0590			GPIO_FUNC15_OUT_SEL_CFG_REG	--- Configuration register for GPIO15 output R/W
$0594			GPIO_FUNC16_OUT_SEL_CFG_REG	--- Configuration register for GPIO16 output R/W
$0598			GPIO_FUNC17_OUT_SEL_CFG_REG	--- Configuration register for GPIO17 output R/W
$059C			GPIO_FUNC18_OUT_SEL_CFG_REG	--- Configuration register for GPIO18 output R/W
$05A0			GPIO_FUNC19_OUT_SEL_CFG_REG	--- Configuration register for GPIO19 output R/W
$05A4			GPIO_FUNC20_OUT_SEL_CFG_REG	--- Configuration register for GPIO20 output R/W
$05A8			GPIO_FUNC21_OUT_SEL_CFG_REG	--- Configuration register for GPIO21 output R/W


$06FC			GPIO_DATE_REG	GPIO version register R/W
( Clock Gate Register )
$062C			GPIO_CLOCK_GATE_REG GPIO clock gate registerR/W







