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
    JSComment(str) -> "//" ++ str.string

fun main(): io ()
  with val alextrace = False
  var done := False
  var s := r#"{"name":"Gpa142749579","age":365380512,"children":[{"name":"another child219059342"}],"adult-children":[]}"#.slice
  while({!done})
    match s.alexScan(0)
      AlexToken(s',len,p) ->
        p(s.subslice(0, len)).show.println
        s := s'
      AlexEOF ->
        "EOF".println
        done := True
      AlexSkip ->
        done := True
      AlexError ->
        done := True