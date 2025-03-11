ESP32

: *TACHYON* ;
{
A Tachyon wordcode Forth written in ESP32Forth
Testing mechanisms and high level debugging
for eventually machine code version
}

$1000 1-	:= LIT12
$2000 1-	:= _IF
$2000 1-
                   	w               = NOCODES	' wordcode offset for 10-bit literals 0..1023
                   	_IF             = w+$400      	' IF relative forward branch 0 to 127 words
                   	_UNTIL          = w+$480      	' UNTIL relative reverse branch 0 to 127 words
                   	_GO		= w+$500
                   	GOBACK		= w+$580
                   	_QIF		= w+$600
                   	rg		= w+$800      	' task/cog register 8-bit offset



long ip
: DONEXT
	ip @ W@ 2 ip +!

	;

*END*
