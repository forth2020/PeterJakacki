( DIRK's DEMO )

: leit  	35 emit ;
: vleit 	cr 0 do leit loop ;
: wu		80 1 do dup i vleit loop drop ;
: wd      	80 1 do dup 80 i - vleit loop drop ;
: wave    	wu wd ;
: waves   	0 do wave loop ;


: wh ( i -- )	1 do dup i vleit loop drop ;
: wr ( i -- )	-1 1 rot do i vleit dup +loop drop ;
: dwaves 	80 2 do i cr wh cr i wr loop ;

Your code for waves as shown on the screen won't run like that.
wu and wd do a "dup" which isn't being used in the loop and neither do you pass a parameter to these words.
Always best to check any code you post, because you know, people might check it 😂

This works.

1 byte dly    0 dly C!
: leit  	35 emit dly C@ 0 max 10 min ms ;
: vleit 	cr 0 do leit loop ;
: wu		80 1 do i vleit loop ;
: wd      	80 1 do 80 i - vleit loop ;
: wave    	wu wd ;
: waves   	0 do wave key? if leave then loop ;


: wh ( i -- )	1 do i vleit loop ;
: wr ( i -- )	-1 1 rot do i vleit dup +loop drop ;
: dwaves 	80 2 do i wh i wr key? if leave then loop ;
: slow		5 dly C! ;
: fast		0 dly C! ;
: faster	dly C@ IF dly C-- THEN ;
: slower	dly C++ ;


TAQOZ

1 byte dly    0 dly C!
: leit  	35 emit dly C@ 0 max 10 min ms ;
: vleit 	crlf 0 do leit loop ;
: wu		80 1 do i vleit loop ;
: wd      	80 1 do 80 i - vleit loop ;
: wave    	wu wd ;
: waves   	0 do wave key if leave then loop ;


: wh ( i -- )	1 do i vleit loop ;
: wr ( i -- )	-1 1 rot do i vleit dup +loop drop ;
: dwaves 	80 2 do i wh i wr key if leave then loop ;
: slow		5 dly C! ;
: fast		0 dly C! ;
: faster	dly C@ IF dly C-- THEN ;
: slower	dly C++ ;






: BIN>BCD ( bin -- bcd )
  0 SWAP BEGIN



;



variable begins 15 cells allot
: >B		begins DUP 4 + 60 CMOVE> begins ! ;
: B> 		begins @ begins DUP 4 + SWAP 60 CMOVE ;
: BEGIN:	R@ >B ;
: UNTIL:	0= IF R> DROP begins @ >R ELSE B> DROP THEN ;

: Q1   		BEGIN: KEY $1B = UNTIL: ;


variable begins
: BEGIN:	R@ begins ! ;
: UNTIL:	0= IF R> DROP begins @ >R THEN ;
: Q1   		BEGIN: KEY $1B = UNTIL: ;




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

: BOUNDS 			OVER + SWAP ;
: fibo ( n -- f )  		0 1 ROT 1- 1- FOR BOUNDS NEXT DROP ;



https://github.com/milkv-duo/duo-arduino/releases/download/V1.0.0/package_sg200x_index.json



FORGET FLAGS
public

$70000 := FLAGS
FLAGS 8190 + := EFLAG

: PRIMES  ( -- n )
    FLAGS 8192 1 FILL
    0 3   EFLAG FLAGS
    DO I C@
         IF DUP I + DUP EFLAG <
            IF  EFLAG SWAP
              DO DUP 0 I C! +LOOP
            ELSE DROP
	    THEN
	    SWAP 1+ SWAP
       THEN 2+
    LOOP DROP
    ;
\ TAQOZ# lap primes lap .lap space . --- 6,323,793 cycles= 19,761,853ns @320MHz 1899  ok


$70000 := FLAGS
FLAGS 8190 + := EFLAG

: PRIMES  ( -- n )
    FLAGS 8192 1 FILL
    0 3   EFLAG FLAGS
    DO I C@
         IF DUP I+ DUP EFLAG <
            IF  EFLAG SWAP
              DO DUP I C~ +LOOP
            ELSE DROP
	    THEN
	    b++
       THEN 2+
    LOOP DROP
    ;

\ TAQOZ# lap primes lap .lap space . --- 5,739,385 cycles= 17,935,578ns @320MHz 1899  ok








8190 := *SIZE
$40000 	:= *FLAGS

: SIEVE
        10 0
	 DO *FLAGS 8190 1 FILL
	 0 8190 0
	  DO I *FLAGS + C@
	   IF 3 I + I + DUP I + *SIZE <
	    IF 8190 *FLAGS + OVER I + *FLAGS +
	     DO 0 I C! DUP +LOOP
	    THEN
	   DROP 1+
	   THEN
	  LOOP
	 CRLF .
	 ."  Primes"
	 LOOP
;

: DEMO   BEGIN 0  $1000 0 DO I C@ $FF = IF 1+ THEN LOOP . CR KEY $1B = UNTIL ;
\      CODE     WORD            TYPE    DESC
1C31C: pub DEMO
0A7A2: 2000     0               CODED   10-bit literal
0A7A4: 007E     4096    $1000   ASM     inline 16-bit literal follows
0A7A8: 2000     0               CODED
0A7AA: 111C     DO              ASM     DO limit+ndex+IP into loop stack
0A7AC: 0147       I             ASM     I reads directly from index register (top 3 of stack)
0A7AE: 0113       C@            ASM
0A7B0: 20FF       255           CODED
0A7B2: 00FD       =             ASM
0A7B4: 2401       IF $A7B8      CODED   conditional branch
0A7B6: 00BB         1+          ASM
                 THEN
0A7B8: 0150     LOOP            ASM     LOOP uses the IP on the LOOP stack to loop (A7AC)
0A7BA: 3578     .               THREAD  High level threaded address (IP > RETURN STACK)
0A7BC: 2A8E     CR              THREAD
0A7BE: 3F56     KEY             THREAD
0A7C0: 201B     27              CODED
0A7C2: 00FD     =               ASM
0A7C4: 2492     UNTIL $A7A2     CODED   Conditional branch b8=neg $A7C6-($12*2)
0A7C6: 0065     ;               ASM     EXIT (pops return stack into IP)
      ( 38  bytes )
 ---  ok


{
<<sieve.fs>>=
7919 2/ CONSTANT maxp      \ 2/ because we only count odd primes
: PRIMES ( -- n )
  HERE maxp 1 FILL
  1 ( count, including 2 )
  maxp 0 DO
    I HERE + C@ IF
      I 2* 3 + ( dup .) DUP  I + ( prime current )
      BEGIN  DUP maxp U<
      WHILE  0 OVER HERE + C!
             OVER +
      REPEAT
      2DROP 1+
    THEN
  LOOP ;
PRIMES .    \ 1000
}


{ TAQOZ OPTIMIZED VERSION
Uses I+ IC~ and DUPE and b++ to avoid stack shuffling
}
FORGET FLAGS
public
$70000 := FLAGS
FLAGS 8190 + := EFLAG

: PRIMES  ( -- n )
    \ FLAGS 8190 1 FILL
    FLAGS 8192 4/ $01010101 LFILL
    0 3   EFLAG FLAGS
    DO IC@
         IF DUP I+ DUPE EFLAG <
            IF  EFLAG SWAP
              DO DUPE IC~ +LOOP
            ELSE DROP
	    THEN
	    b++
       THEN 2+
    LOOP DROP
    ;

CRLF LAP PRIMES LAP .  SPACE .LAP
\ 1899  3,886,993 cycles= 12,146,853ns @320MHz ok


{
DUP +LOOP	1899  4,354,361 cycles= 13,607,378ns @320MHz ok
DUPE +LOOP	1899  3,994,457 cycles= 12,482,678ns @320MHz ok
DUPEs		1899  3,939,665 cycles= 12,311,453ns @320MHz ok
mod depth	1899  3,877,673 cycles= 12,117,728ns @320MHz ok
mod popx	1899  3,872,057 cycles= 12,100,178ns @320MHz ok



}


8190 constant *SIZE
variable *FLAGS *SIZE ALLOT
: SIEVE
        10 0
	 DO *FLAGS 8190 1 FILL
	 0 8190 0
	  DO I *FLAGS + C@
	   IF 3 I + I + DUP I + *SIZE <
	    IF 8190 *FLAGS + OVER I + *FLAGS +
	     DO 0 I C! DUP +LOOP
	    THEN
	   DROP 1+
	   THEN
	  LOOP
	 CR . ."  Primes"
	 LOOP
;

