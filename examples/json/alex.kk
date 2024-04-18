import parsers/alex/json-parse
import std/data/json

fun show(j: jslex): string
  match j
    JSStr(str) -> str.string
    json-parse/JSNum(num) -> num.string
    JSTrue -> "true"
    JSFalse -> "false"
    json-parse/JSNull -> "null"
    JSObjOpen -> "{"
    JSObjColon -> ":"
    JSObjClose -> "}"
    JSArrayOpen -> "["
    JSArrayClose -> "]"
    JSValueSep -> ","
    JSWhite -> " "

fun main(): <io,alexEff> ()
  val done = ref(False)
  val s = ref(r#"{"name":"Gpa142749579","age":365380512,"children":[{"name":"another child219059342"}],"adult-children":[]}"#.slice)
  while({!(!done)})
    thing(s, done)

fun thing(s: ref<h,sslice>, done: ref<h,bool>): <io,alexEff> ()
  match (!s).alexScan(0)
    AlexToken(s',_,p) ->
      // p(s').show.println
      s := s'
    AlexEOF ->
      // "EOF".println
      done := True
    AlexSkip ->
      // "skip".println
      done := True
    AlexError ->
      // "error".println
      done := True