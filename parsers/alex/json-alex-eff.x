{
import std/data/json
import std/core-extras
import std/num/float64

effect jsparse
  fun do-emit(j: json): ()
  fun start-object(): ()
  fun start-array(): ()
  fun finish-object(): ()
  fun finish-array(): ()
  fun add-key(s: string): ()

fun emit(j: json): alex-eff ()
  do-emit(j)
  pop-state()
  ()

}

%encoding "utf8"
%effects "jsparse"
%wrapper "effect"

-----------------------------------------------------------
-- Character sets
-----------------------------------------------------------
$exp          = [eE]
$digit        = [0-9]
$onenine      = [1-9]
$sign         = [\+\-]
$ws           = [\ \t\n\r]
$hex          = [0-9a-fA-F]
$space        = [\ ]
$tab          = [\t]
$return       = \r
$linefeed     = \n
$graphic      = [\x21-\x7E]
$cont         = [\x80-\xBF]

-----------------------------------------------------------
-- Regular expressions
-----------------------------------------------------------
@digits       = $digit+
@exponent     = $exp $sign? @digits
@fraction     = '.' @digits
@integer      = $sign $digit
              | $sign $onenine @digits
              | $digit
              | $onenine @digits

@number       = @integer @fraction? @exponent?
@escape       = \" | '\\' | '\/' | 'b' | 'f' | 'n' | 'r' | 't' | 'u' $hex $hex $hex $hex
@character    = [\x0020 - \x10FFFF] # [\"\\] | '\\' @escape
@string       = \" @character* \"
@newline      = $return?$linefeed
@utf8valid    = [\xC2-\xDF] $cont
              | \xE0 [\xA0-\xBF] $cont
              | [\xE1-\xEC] $cont $cont
              | \xED [\x80-\x9F] $cont
              | [\xEE-\xEF] $cont $cont
              | \xF0 [\x90-\xBF] $cont $cont
              | [\xF1-\xF3] $cont $cont $cont
              | \xF4 [\x80-\x8F] $cont $cont
@utf8         = @utf8valid
@commentchar  = ([$graphic$space$tab] # [\/\*])|@newline|@utf8
@whitespace   = [$space$tab]+|@newline
@linechar     = [$graphic$space$tab]|@utf8
-----------------------------------------------------------
-- Main tokenizer
-----------------------------------------------------------
program :-
-- white space
<0> "//" @linechar* @newline { fn() () }
<0> @whitespace              { fn() () }
<0> @number                  { fn() emit(JSNum(parse-float64(get-string()).unjust)) }
<0> "true"                   { fn() emit(JSBool(True)) }
<0> "false"                  { fn() emit(JSBool(False)) }
<0> "null"                   { fn() emit(JSNull) }
<0> @string                  { fn() emit(JSString(get-slice().advance(1).extend(-2).string)) }
<0> "{"                      { fn() {push-state(objectfield); start-object()}}
<0> "["                      { fn() {push-state(array); start-array()} }

<objectfield> @string        { fn() add-key(get-slice().advance(1).extend(-2).string) }

<object> ","                 { fn() push-state(objectfield); }
<object> ":"                 { fn() push-state(0) }
<object> "}"                 { fn() {pop-state(); finish-object()} }

<array> ","                  { fn() push-state(0); }
<array> "]"                  { fn() {pop-state(); finish-array()} }
