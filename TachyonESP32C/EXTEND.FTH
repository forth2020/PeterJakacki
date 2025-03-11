( EXTEND is mainly designed for the ESP32-C3 and for compatibility with Tachyon tools )
\ FORGET *EXTEND*
: *EXTEND*	."  TachyonESP extensions 240929-1400 " cr ;
( very simple batch header - WIP - ./send EXTEND ACM0 3 )
variable batch 	variable line# 		internals
: .LINE# 	line# @ 4 U.R SPACE ; : PROMPTL $0D EMIT line# ++ .line# ;
: ESP32 	HERE batch ! errors ~ echo ~  ['] noop to _eol ['] accept1 to _accept
		['] PROMPTL to _prompt line# ~ !SP Forth ;
( -------------------------------------------------------------------------------------- )
ESP32	( enter batch mode for ESP32 )
forth

\ multi-line block comment up to a matching nested }
: { 	1 BEGIN KEY DUP '{' = IF DROP 1+
	ELSE [CHAR] } = IF 1- THEN THEN DUP 0= UNTIL DROP ;
	IMMEDIATE

\ my prefered comment separates and doesn't look like a maths operator --- maybe :)
: ---		10 parse drop drop ; immediate
--- a separator for readability even in posted code (i.e. FB removes extra spaces)
: ... ; immediate


	( ALIASES )


: pri : ;	--- private def that can be stripped from the dictionary during a reclaim
: pub : ;	--- public def - left intact during a reclaim


( Tachyon ALIAS operators to symbols, and symbols to function names )
: <<			lshift ;
: >>			rshift ;
: &			  AND ;
: !&			INVERT & ;
: &!			AND 0= ;
: |			  OR ;
: ^			  XOR ;
: :=			constant ;

( convert bit to bitmask )
: |< ( b -- bm )	1 SWAP << ;
( convert bitmask to bit )
: >| ( bm -- b ) 	-1 SWAP BEGIN ?DUP WHILE 1 >> SWAP 1+ SWAP REPEAT ;
: C@+ 			dup 1+ swap c@ ;
: C+!			dup C@ rot + swap C! ;
: C++			1 swap C+! ;
: C--			-1 swap C+! ;
: C~			0 swap C! ;
: n3			SP@ 8 - @ ;
: n4			SP@ 12 - @ ;


: PRINT"		postpone s" state @ if postpone type else type then ; immediate
: "			postpone s" state @ if postpone drop else drop then ; immediate ( -- zstr )
: ms@			ms-ticks ;
: KB ( KB -- bytes )	10 << ;


: UTF8 ( code -- )	$E2 EMIT DUP 6 >> $80 + EMIT $3F AND $80 + EMIT  ;
: DINGBAT ( code -- )  	$E2 EMIT DUP 6 >> $9C + EMIT $3F AND $80 + EMIT ;
: EMOJI ( code -- )    	$F0 EMIT $9F EMIT DUP 6 >> $98 + EMIT $3F AND $80 + EMIT ;
{
defer EMITTER  ' EMOJI is EMITTER
: EMS ( from cnt -- )	BOUNDS DO I $0F &! IF CR I .W SPACE THEN I EMITTER SPACE LOOP ;
}
: EMITS ( ch cnt -- )	1- FOR DUP EMIT NEXT DROP ;
: CURSOR ( on/off -- )	ESC[ '?' EMIT 25 PRINT IF 'h' ELSE 'l' THEN EMIT ;
: CLS			^L EMIT ;

: ?EXIT			IF R> DROP EXIT THEN ;
: 0EXIT			0= IF R> DROP EXIT THEN ;




internals
: ?emit			?echo ;
forth

( null term strings )
: LEN$ ( adr -- cnt )	dup begin c@+ 0= until swap - 1- ;
: TYPE$ ( addr -- )	begin C@+ ?dup while emit repeat drop ;

( WITHIN - INCLUSIVE )
: >< ( n min max -- f )	ROT SWAP ( min n max ) 1+ OVER > ( min n f ) -ROT 1+ < & ;


	( DATASPACE )
variable @org
: org			@org ! ;
HERE org  4 KB ALLOT
@org @			:= org0
: !org			org0 org ;
--- align org if needed
: &org ( n -- )		1- @org @ OVER + SWAP !& org ;

: bytes			@org @ constant @org +! ;
: byte			1 bytes ;
( align longs to cell boundary )
: longs			cell &org cells bytes ;
: long			1 longs ;


	( AUX STACK )
{
simple aux stack - 16 levels - won't error on over or underflow
Also manipulate the tos via @A such as @A ++
Can also be used as a locals stack - use A B C - no need to pop either
usage:
0#> 12 34 56 78    ---  ok
4#> .S ---
1   0000004E  N  78
2   00000038  8  56
3   00000022  "  34
4   0000000C  .  12  ok
4#> 2 PUSHA ---  ok
2#> .S ---
1   00000022  "  34
2   0000000C  .  12  ok
2#> A . --- 56  ok
2#> B . --- 78  ok
2#> 2 POPA  ---  ok
4#> .S ---
1   0000004E  N  78
2   00000038  8  56
3   00000022  "  34
4   0000000C  .  12  ok
4#>
}

16 		longs @A
@A CELL+	:= @B
@B CELL+	:= @C
@C CELL+	:= @D

: A!		  @A ! ;
: A		    @A @ ;
: B		    @B @ ;
: C		    @C @ ;
: D		    @D @ ;
: >A		  @A @B 60 CMOVE> A! ;
: DROPA		@B @A 60 CMOVE ;
: A> 		  A DROPA ;

{
so rather than use the return stack for temps -
just push and pop multiples using aux stack
}
: PUSHA ( <n> cnt -- ) 	1- FOR >A NEXT ;
: POPA ( cnt -- <n> )	  1- FOR A> NEXT ;

( AUX STACK EXTRAS )
: A@+		        A @ CELL @A +! ;
: AW@+ ( -- w )	A UW@ 2 @A +! ;
: AC@+ ( -- b )	A C@ @A ++ ;



	( SIMPLE SWITCH CASE )

long _?
: ?! ( n -- )		_? ! ;
: ?@ ( -- n )		_? @ ;
: ?= ( n -- f )		?@ = ;
: ?>< ( min max -- f )	?@ -ROT >< ;




	( TIMING TOOLS )

long lap1  long lap2
: LAP			ms@ lap1 @ lap2 ! lap1 ! ;
: .LAP			lap1 @ lap2 @ - ABS . PRINT" ms" ;


( colors: black red green yellow blue magenta cyan white )


: .COLOR ( xt -- xt )
	plain blue pen DUP CELL - C@ ?DUP
	IF
	  DUP 1 = IF bold red pen DROP ELSE IF magenta pen THEN THEN	\ a flag?  immediate red or else blue
	ELSE
	  DUP @ ['] red @ = IF green pen THEN	\ green constants
	  DUP @ ['] lap1 @ = IF yellow pen THEN	\ yellow variables
	  DUP @ ['] LAP @ = IF plain THEN	\ cyan colon defs
	THEN
	;


: .words
    cr
    context @ @
    BEGIN  DUP
    WHILE plain ( ?ins. ) DUP .ADDR DUP @ .L 2 SPACES
      .COLOR
      DUP >NAME TYPE >link
    REPEAT
    DROP cr
;

byte _xpos byte _xmax byte _ymax

: ?WRAP ( cnt -- )	DUP _xpos C@ + 2 + _xmax C@ > if cr _xpos C~ then 1+ _xpos C+! ;

: words ( <from> -- )
    _xpos C~
    cr bl parse find ?DUP 0= IF context @ @ THEN
    BEGIN DUP
    WHILE plain ( ?ins. ) .COLOR DUP >NAME DUP ?WRAP TYPE SPACE >link
    REPEAT
    DROP plain bold yellow pen cr
;



	( *** MEMORY DUMPS *** )

: .ADDR			CR .L ." :  "  ;
: ?ADDR			over $0F &! if dup .addr then over 3 &! if space then drop ;
: atype	( a n -- )	bounds do i c@ aemit loop ;
\ : ?ASCII		dup $0f &! if 4 spaces I 15 - 16 atype then ;

: dump
    0 -rot bounds do
      I ?addr I c@ .B space
      1+ dup $0f &! if 4 spaces I 15 - 16 atype then
    loop drop
;
--- quick dump
: qd ( addr -- )    $20 dump ;
--- dump words
: dumpw
    0 -rot bounds do
     I ?addr I uw@ .w space  2 +
    2 +loop drop
;
--- dump longs
: dumpl
    0 -rot bounds do
     I ?addr I @ .L space  2 +
    4 +loop drop
;
--- dump ascii
: dumpa	    bounds do i .addr  I 64 atype 64 +loop ;

	( MEMORY MAP VIEWER )
--- dump blocks
: dumpb ( addr cnt blksiz -- )
	-ROT BOUNDS DO
	  I OVER 4 << 1- &! IF i .ADDR THEN
	  I OVER BOUNDS 0 -ROT DO I C@ + LOOP
	OVER / .B SPACE DUP +LOOP DROP
;
--- dump code longs
: dumpc ( addr cnt -- )
	swap >A 1- FOR A .ADDR A@+ .L NEXT
;




	( STACK PRINT )

\ Enhanced stack print in top to bottom multi-mode format
: ..s	cr over . 2 spaces dup .l 2 spaces dup aemit 2 spaces . ;
: .s  	depth if sp@ >A 1 begin A sp0 >
	while A @ ..s -4 @A +! 1+ repeat
	A> 2drop else ."  empty " then ;




	( CONSOLE ENHANCEMENTS )


--- timeout if no key pressed
: ?KEY ( ms -- key|0 )	ms@ + BEGIN KEY? 0= WHILE DUP ms@ = IF DROP 0 EXIT THEN REPEAT DROP KEY ;

--- wait for key input and getnum - terminate on timeout or invalid key
: ?NUM ( ms -- n )
    0 BEGIN OVER ?KEY DUP '0' '9' ><
    WHILE '0' - SWAP 10 * +
    REPEAT DROP NIP
;
	( ANSI SETUP )

--- ESC[6n 	request cursor position (reports as ESC[#;#R)
: XY@ ( -- x y )
    ESC[ '6' EMIT 'n' EMIT
    5 ?KEY $1B = IF 5 ?KEY DROP 5 ?NUM 5 ?NUM SWAP ELSE 0 0 THEN
;
--- if ANSI, the terminal will respond to this with its window size - otherwise 0 0
: ANSI@ ( -- xmax ymax )
    XY@ 999 999 XY XY@ >R >R XY R> R>
    ;
--- Check ANSI support and set line-width else disable ANSI
: ?ANSI		1 to _ansi ANSI@ _ymax C! ?DUP 0= IF 0 to _ansi 80 THEN _xmax C! ;



long _keys	--- maintain last 4 keys
( read a CSI sequence and abbreviate to 32-bits )
: CSIKEY ( --- code )
    KEY DUP $1B =
    IF  ms@ SWAP
      BEGIN KEY?
---              ignore ;      else merge to code       update timeout
        IF KEY DUP ';' = IF DROP ELSE SWAP 8 << OR THEN   NIP ms@ SWAP THEN
---     timeout after 3ms of inactivity - must be end of sequence
        OVER ms@ - ABS 5 >
      UNTIL NIP
    ELSE
      _keys @ 8 << OVER + _keys !  --- save previous keys
    THEN ;


: .BOOT			?ANSI cls plain bold green pen reverse *EXTEND* plain ok plain ;



: syspen		plain blue pen ;
: usrpen		plain bold yellow pen space 8 emit ;
: uprompt    		$0d emit syspen depth str type '#' emit '>' emit space usrpen  ;


	( *** CONSOLE EDIT & HISTORY *** )


8 	:= #hists
128	:= tibmax

tibmax #hists * bytes historys

long adr : adr@ adr @ ;
--- note: cnt,aflg,hist#, are part of a cell and can be cleared with one operation
byte cnt : cnt@ cnt C@ ;	--- accept cnt (mainly write index)
byte aflg
byte hist#
byte mx

: @history ( n -- a )	tibmax * historys + ;
			\ copy selected history                                 to tib               and set cnt
: history>		historys  tibmax hist# C@ #hists MIN DUP IF 1- THEN * +  adr@ tibmax cmove adr@ len$ cnt C! ;
			\ if new line    then push historys down                             and copy buffer to latest history
: >history		hist# C@ 0= IF historys  historys tibmax + tibmax #hists 1- * CMOVE> adr@ historys tibmax cmove THEN ;
: delhist# ( n -- )
	1- dup
	@history dup tibmax + swap ( # src dst )
	rot #hists swap - ( s d n ) tibmax * cmove
	#hists 1- @history tibmax erase
;
: reprompt		$0D emit _xmax C@ 20 max 128 min spaces uprompt ;
: clrtib		adr@ tibmax erase cnt C~ ;
: retype		reprompt adr@ type$ ;
: gethist		history>   retype ;
: delhist		hist# C@ delhist# gethist ;
: .tib 			uprompt adr@ type$   XY@ NIP cnt@ 5 + swap XY ;


			\ retype line if modified
: =CR			cnt@ adr@ len$ <> IF retype THEN    syspen ."  --- "  plain hist# C~    ;
: =UP			hist# C++ gethist 1 aflg C! ON CURSOR ;
: ?INS			aflg C@ 1 & if adr@ cnt@ + dup 1+ over len$ cmove> .tib then  ;
: =CH			?INS ?@ dup ?emit adr@ cnt@ + C! cnt C++ hist# C~ ;

	( key handlers )
: ?keys
    ?!
    ^I ?= ( TAB )	if $20 ?! then
    $20 $7E ?>< cnt@ mx C@ < & if =CH exit then
---			   len>3 and no more keys then save history
    ^M ?= ( CR )	if adr@ len$ 3 > key? 3 < and if >history then =CR exit then
    ^J ?= ( LF )	if exit then
    --- shortcuts ---
    ^H ?= ( BS )	if cnt@ if cnt C-- 8 ?emit bl ?emit 8 ?emit $20 adr@ cnt@ + C! then then
    ^Q ?= ( .S ) 	if .s cr uprompt then
    ^S ?= ( !SP )	if !sp uprompt then
    ^W ?= ( WORDS )	if words uprompt then
    ^L ?= ( cls )	if ^L emit 50 ms .BOOT !sp then
    ^C ?= ( cancel )	if bye then
    ^Z ?= ( ZAP )	if _keys @ $1A1A1A1A = if reset bye then then
    ^? ?= ( QUERY )	if ok then
    ^X ?= ( execute )	if =UP =CR $0D ?! then
    ^[ ?= ( esc )	if clrtib reprompt then
    --- ANSI KEYS ---
    $1B5B41 ?= ( UP )	if =UP then
    $1B5B42 ?= ( DN )	if hist# C@ if hist# C-- hist# C@ if gethist exit then then clrtib reprompt then
    $1B5B43 ?= ( -> )	if cnt@ adr@ len$ < if cnt C++ .tib then then
    $1B5B44 ?= ( <- )	if cnt@ IF cnt C-- .tib ON CURSOR then then
    $1B5B40 ?= ( INS )	if aflg C++ then
    $1B5B48 ?= ( home )	if cnt C~ .tib then
    $1B4F46 ?= ( end )	if adr@ len$ cnt C! .tib then
    $1B7F ?=   ( del )	if adr@ len$ cnt@ = if delhist else   adr@ cnt@ + dup 1+ swap over len$ 1+ cmove reprompt .tib then then
;
forth


: accept2 ( adr max -- cnt )
   2dup erase cnt ~ mx C! adr !
   usrpen begin csikey ?keys ... ?@ $0a = ?@ $0d = or until
   adr@ len$
;


	( *** TESTING COMPEX *** )
{
COMPEX (Compile and Execute) is used in Tachyon in place of REPL
which normally interprets a line where each word is evaluated and executed
before moving on to the next word.
Instead, at the start of a line the code pointer HERE is saved in THERE,
then each word is compiled as if it were in a definition and HERE increments.
At the end of line an EXIT is added and THERE is recalled and executed,
after which HERE is restored from THERE. Simple!
Testing only - need to replace the REPL loop yet.
}
0 value THERE

: [[ 		HERE to THERE ['] DUMP @ , ['] ] CALL ;
: ]] 		['] [ CALL ['] EXIT , THERE CALL THERE HERE - ALLOT ; immediate

\ [[ $7F $20 DO I EMIT LOOP ]]


	( BOOT TIME )


internals
: *END*
    echo ~~ ['] eolok to _eol !prompt
    CR ." Batch load completed - "
    HERE batch @ - ABS . ." bytes over " .LINE# ." lines"
    errors @ ?dup if cr red pen . ." errors!" then plain
    ['] accept2 to _accept
    ;
forth
: EXTEND			200 ms !SP ['] accept2 to _accept .BOOT ;


*END*
remember startup: EXTEND  \ disable while testing
EXTEND
