{
import std/data/json
import std/core/bslice
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
alias alex-input = sslice
alias alex-user = ()
fun alexGetByte(s: alex-input): maybe<(char, alex-input)>
  s.next()

fun alexInputPrevChar(s: alex-input): char
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
$graphic      = [\x0020 - \x10FFFF] # [\r\n]
$whitespace   = [\x09\x0A\x0B\x0C\x0D\x20\x85\xA0\x1680\x2000-\x200A\x2028\x2029\x202F\x205F\x3000]
$return       = [\x0A\x0B\x0C\x0D\x85\x2028\x2029]
$whitespacenoreturn = $whitespace # $return
$graphicnoreturn = $graphic # $return

-----------------------------------------------------------
-- Regular expressions
-----------------------------------------------------------
@digits       = $digit+
@exponent     = $exp $sign? @digits
@fraction     = \. @digits
@integer      = $sign $digit
              | $sign $onenine @digits
              | $digit
              | $onenine @digits

@number       = @integer @fraction? @exponent?
@escape       = \" | \\ | \/ | b | f | n | r | t | (u $hex $hex $hex $hex)
@character    = ([\x0020 - \x10FFFF] # [\"\\]) | \\ @escape
@newlines     = $return*
@linechar     = $graphicnoreturn
-----------------------------------------------------------
-- Main tokenizer
-----------------------------------------------------------
program :-
-- white space
<0> "//" @linechar* @newlines   { fn(s:sslice) JSComment(s.advance(2)) }
<0> $whitespace           { fn(s:sslice) JSWhite }
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
<0> \" @character* \"  { fn(s:sslice) JSStr(s.advance(1).extend(-2)) }

{
}