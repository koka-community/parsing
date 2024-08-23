{
import std/data/json
import std/core-extras
import std/num/float64

type jslex
  JSStr(str: sslice)
  JSNum(num: sslice)
  JSComment(str: sslice)
  JSTrue
  JSFalse
  JSNull
  JSObjOpen
  JSObjColon
  JSObjClose
  JSArrayOpen
  JSArrayClose
  JSValueSep
  JSWhite

alias action = sslice -> pure jslex
alias alexInput = sslice
alias alexUser = ()
fun alexGetByte(s: alexInput): maybe<(char, alexInput)>
  s.next()

fun alexInputPrevChar(s: alexInput): char
  '_'

}

%encoding "utf8"
%wrapper "no-effect"

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
<0> "//" @linechar* @newline   { fn(s:sslice) JSComment(s.advance(2)) }
<0> @whitespace           { fn(s:sslice) JSWhite }
<0> @number               { fn(s:sslice) JSNum(s) }
<0> "true"                 { fn(s:sslice) JSTrue }
<0> "false"                { fn(s:sslice) JSFalse }
<0> "null"                 { fn(s:sslice) JSNull }
<0> ","               { fn(s:sslice) JSValueSep }
<0> "{"           { fn(s:sslice) JSObjOpen }
<0> "}"          { fn(s:sslice) JSObjClose }
<0> ":"            { fn(s:sslice) JSObjColon }
<0> "["            { fn(s:sslice) JSArrayOpen }
<0> "]"           { fn(s:sslice) JSArrayClose }
<0> @string               { fn(s:sslice) JSStr(s.advance(1).extend(-2)) }

{
}