import std/data/json
import std/text/regex
import parsing/regex-parser
import std/num/float64

val leadingHexDotRegex = r"[+-]?0[xX]\.[0-9a-fA-F_]+([eEpP][+-]?[0-9a-fA-F_]+)?".regex() 
val wholeHexFloatRegex = r"[+-]?0[xX][0-9a-fA-F_]+(\.[0-9a-fA-F_]+)?([eEpP][+-]?[0-9a-fA-F_]+)?".regex() 
val integerRegex = r"[+-]?[0-9]+".regex() 
fun integer()
  takePattern(integerRegex)

fun exp()
  integer()

fun decfloat()
  (fn() takePattern(wholeFloatRegex)).or({takePattern(leadingDotRegex)})

fun hexfloat()
  (fn() takePattern(wholeHexFloatRegex)).or({takePattern(leadingHexDotRegex)})

fun pLit()
  val x = ors([{text("true"); JSBool(True)},{text("false"); JSBool(False)},{text("null"); JSNull}])
  optional(pWhitespace)
  x

fun pFloat()
  val d = or({decfloat().string.split("_").join}, {hexfloat().string.split("_").join})
  optional(pWhitespace)
  // trace(d)
  JSNum(d.parse-float64.unjust)

fun pInteger()
  val d = integer().string.split("_").join
  optional(pWhitespace)
  // trace(d)
  JSInt(d.parse-int.unjust)

val stringRegex = r#""[^\\\"]*(?:\\.[^"\\]*)*""#.regex()

inline fun takeString()
  val s = takePattern(stringRegex)
  s.advance(1).extend(-2).string

fun pString()
  val s = takeString()
  optional(pWhitespace)
  JSString(s)

fun pArray()
  pToken("[")
  with scope("parray")
  val xs = separatedTrailing(pValue,{pToken(",")})
  pToken("]")
  JSList(xs)

fun pMember(): <div,exn,parse<string>> (string,json)
  val k = takeString()
  with scope("pmember")
  optional(pWhitespace)
  pToken(":")
  val v = pValue()
  (k, v)

fun pObject() 
  pToken("{")
  with scope("pobject")
  val members = separatedTrailing(pMember, {pToken(",")})
  val o = JSObj(members)
  pToken("}")
  o

fun pValue(): <div,exn,parse<string>> json
  ors([pArray,pObject,pString,pFloat,pInteger,pLit])

val spacesRegex = r"([ \n\t\r]|\/\/[^\n]+)+".regex()
fun pWhitespace()
  spacesRegex.takePattern()

fun pToken(s: string)
  text(s)
  optional(pWhitespace)

fun str/parse(s: string)
  match s.run(pValue)
    Right(e) -> e
    Left(err) -> throw("Error:\n" ++ err.show(s))