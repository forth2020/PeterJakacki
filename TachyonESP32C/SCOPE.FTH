\ FORGET *SCOPE*
ESP32

: *SCOPE* 	." VERY VERY SIMPLE ESP32-C3 SCOPE " ;
{
While checking out the ADC, I decided to graph it better on an ANSI terminal.
That worked well so I added a capture buffer, and then some controls.
Start it up with the channel and sampling time (us) parameters.
For Channel 0 and fastest = 0:  0 0 SCOPE
The trigger will be on auto and rising edge initially -
use 50% to set trigger halfway
Use - to set falling edge or + to default to rising
}

512 bytes samples
2 bytes _trig
2 bytes _mode
: .INFO		1 1 XY PRINT" CH" OVER 1 U.R 1 2 XY PRINT" *" dup;
: us		?DUP IF 56 - 0 MIN 3 / ?DUP IF 0 DO LOOP THEN THEN ;
: CAPTURE	samples 512 BOUNDS DO OVER ADC I W! DUP us 2 +LOOP ;
: RISING	2 FOR BEGIN n3 ADC OVER > KEY? OR UNTIL 96 + NEXT DROP ;
: FALLING	2 FOR BEGIN n3 ADC OVER < KEY? OR UNTIL 96 - NEXT DROP ;
: TRIG		_trig UW@ ?DUP IF _mode C@ IF FALLING ELSE RISING THEN THEN ;
: FIT		4096 _ymax C@ / / ;
: PLOT		_ymax C@ SWAP - XY '*' EMIT ;
: HOLDOFF	300 ms ;
: .GRID		plain 1 _ymax C@ 2/ XY   '-' _xmax C@ EMITS ;
: TRACE		OVER bold yellow + pen ;
: .SAMPLES 	_xmax C@ 4 - FOR I 5 + I 2* samples + UW@ FIT PLOT NEXT ;
: .SCOPE	CLS .GRID TRACE .SAMPLES ;
: PAUSED	_xmax C@ 2/ 3 - _ymax C@ 2/ XY ."  PAUSED " ;
( CONTROLS )
long num        --- entry number
: CLEAR         num ~ ;
: +NUM		'0' - num @ 10 * + num ! ;
: NUM@          num @ CLEAR ;
: ?BUTTONS
    KEY? 0EXIT
    KEY DUP 'a' 'z' >< IF $20 - THEN ?!
    $08 ?= IF CLEAR THEN		--- clear number
    '0' '9' ?>< IF ?@ +NUM THEN		--- enter number
    '-' ?= IF _mode C++ THEN		--- falling edge
    '+' ?= IF _mode C~ THEN		--- rising edge
    '%' ?= IF NUM@ 40 * _trig W! THEN 	--- trigger %
    '*' ?= IF DROP NUM@ THEN 		--- timebase us
    '#' ?= IF NIP NUM@ SWAP THEN 	--- channel #
    $20 ?= IF PAUSED BEGIN KEY UNTIL THEN
    $1B ?= IF R> DROP 2DROP THEN
;
: RUN		BEGIN .INFO TRIG CAPTURE .SCOPE HOLDOFF ?BUTTONS AGAIN ;
--- USAGE: ch us SCOPE  i.e. 0 0 SCOPE
: SCOPE 	OFF CURSOR _trig ~ RUN ON CURSOR ;

{
( TESTING DIRECT ACCESS TO ADC HARDWARE )

: SARADC	$6004_0000 + ;
: ADC@		$2C SARADC @ ;

}

*END*
