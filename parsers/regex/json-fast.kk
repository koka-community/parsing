import std/data/json
import std/text/regex
import parsing/regex-parser
import std/num/float64

val leadingHexDotRegex = r"[+-]?0[xX]\.[0-9a-fA-F_]+([eEpP][+-]?[0-9a-fA-F_]+)?".regex() 
val wholeHexFloatRegex = r"[+-]?0[xX][0-9a-fA-F_]+(\.[0-9a-fA-F_]+)?([eEpP][+-]?[0-9a-fA-F_]+)?".regex() 
val integerRegex = r"[+-]?[0-9]+".regex() 
fun parse-int()
  take-pattern(integerRegex)

fun parse-exp()
  parse-integer()

fun parse-decfloat()
  (fn() take-pattern(wholeFloatRegex)).or({take-pattern(leadingDotRegex)})

fun parse-hexfloat()
  (fn() take-pattern(wholeHexFloatRegex)).or({take-pattern(leadingHexDotRegex)})

fun parse-literal()
  val x = ors([{text("true"); JSBool(True)},{text("false"); JSBool(False)},{text("null"); JSNull}])
  optional(parse-whitespace)
  x

fun parse-float()
  val d = or({parse-decfloat().string.split("_").join}, {parse-hexfloat().string.split("_").join})
  optional(parse-whitespace)
  // trace(d)
  JSNum(d.parse-float64.unjust)

fun parse-integer()
  val d = parse-int().string.split("_").join
  optional(parse-whitespace)
  // trace(d)
  JSInt(d.parse-int.unjust)

val stringRegex = r#""[^\\\"]*(?:\\.[^"\\]*)*""#.regex()

inline fun parse-str()
  val s = take-pattern(stringRegex)
  s.advance(1).extend(-2).string

fun parse-string()
  val s = parse-str()
  optional(parse-whitespace)
  JSString(s)

fun parse-array()
  parse-token("[")
  with scope("parse-array")
  val xs = parse-sep-trailing(parse-value,{parse-token(",")})
  parse-token("]")
  JSList(xs)

fun parse-member(): <div,exn,parse<string>> (string,json)
  val k = parse-str()
  with scope("pmember")
  optional(parse-whitespace)
  parse-token(":")
  val v = parse-value()
  (k, v)

fun parse-object() 
  parse-token("{")
  with scope("parse-object")
  val members = parse-sep-trailing(parse-member, {parse-token(",")})
  val o = JSObj(members)
  parse-token("}")
  o

fun parse-value(): <div,exn,parse<string>> json
  ors([parse-array,parse-object,parse-string,parse-float,parse-integer,parse-literal])

val spacesRegex = r"([ \n\t\r]|\/\/[^\n]+)+".regex()
fun parse-whitespace()
  spacesRegex.take-pattern()

fun parse-token(s: string)
  text(s)
  optional(parse-whitespace)

fun str/parse(s: string)
  match s.run(parse-value)
    Right(e) -> e
    Left(err) -> throw("Error:\n" ++ err.show(s))