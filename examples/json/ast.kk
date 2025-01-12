import parsers/alex/json-lex
import parsing/ast-simple
import std/data/json
import std/data/linearmap
import std/num/float64
import std/pretty/pprint

fun lex/show(t: jslex): string
  match t
    JSStr(s) -> "JSStr(" ++ s.string ++ ")"
    json-lex/JSNum(s) -> "JSNum(" ++ s.string ++ ")"
    JSComment(s) -> "JSComment(" ++ s.string ++ ")"
    JSTrue -> "JSTrue"
    JSFalse -> "JSFalse"
    json-lex/JSNull -> "JSNull"
    JSObjOpen -> "JSObjOpen"
    JSObjColon -> "JSObjColon"
    JSObjClose -> "JSObjClose"
    JSArrayOpen -> "JSArrayOpen"
    JSArrayClose -> "JSArrayClose"
    JSValueSep -> "JSValueSep"
    JSWhite -> "JSWhite"

fun parse-whitespace()
  parse-many("Whitespace")
    parse-token("Whitespace Char") fn(t)
      match t
        JSWhite -> Just(())
        _ -> Nothing

fun parse-trailing-whitespace(f: () -> ast<jslex> a): ast<jslex> a
  val x = f()
  parse-whitespace()
  x

fun parse-json-value()
  parse-trailing-whitespace
    parse-choices("JSValue", [parse-array, parse-object, parse-null, parse-bool, parse-number, parse-string])

fun is-token(nm: string, b: (jslex) -> bool): ast<jslex> jslex
  parse-trailing-whitespace 
    parse-token(nm) fn(t: jslex)
      if t.b then Just(t) else Nothing

fun map-token(nm: string, b: (jslex) -> bool, m: (jslex) -> pure a): ast<jslex> a
  parse-trailing-whitespace
    parse-token(nm) fn(t)
      if t.b then Just(m(t)) else Nothing

fun parse-null()
  with _ <- map-token("Null", is-jsnull)
  json/JSNull

fun parse-bool()
  with b <- map-token("Bool", fn(t) t.is-jstrue || t.is-jsfalse)
  match b
    JSTrue -> JSBool(True)
    JSFalse -> JSBool(False)

fun parse-array-open()
  is-token("List start", is-jsarrayOpen)

fun parse-array-close()
  is-token("List end", is-jsarrayClose)

fun parse-object-open()
  is-token("Object start", is-jsobjOpen)

fun parse-object-close()
  is-token("Object end", is-jsobjClose)

fun parse-comma()
  is-token(",", is-jsvalueSep)

fun parse-colon()
  is-token(":", is-jsobjColon)

fun parse-str()
  map-token("string", is-jsstr, fn(JSStr(s)) s.string)

fun parse-number()
  with n <- map-token("num", json-lex/is-jsnum)
  val json-lex/JSNum(s) = n
  val s' = s.string
  match parse-int(s') 
    Just(i) -> JSInt(i)
    Nothing -> 
      match parse-float64(s')
        Just(f) -> json/JSNum(f)
        _ -> throw("Invalid int " ++ s')

fun parse-string()
  JSString(parse-str())

fun parse-array()
  parse-trailing-whitespace
    p-braced(parse-array-open, parse-array-close)
      JSList(p-sep-by("Array Entry", parse-json-value, parse-comma))

fun parse-object()
  parse-trailing-whitespace
    p-braced(parse-object-open, parse-object-close) 
      JSObj(p-sep-by("Obj Entry", parse-object-entry, parse-comma))

fun parse-object-entry()
  val nm = parse-str()
  parse-colon()
  (nm, parse-json-value())

fun json/parse(s: string): <console,pure> json
  with final ctl ast-error(e)
    e.throw
  lex-parse(s, parse-json-value, ?lex=lex)

fun lex(s: string): pure list<jslex>
  lex-recur(s.slice)

fun lex-recur(s: sslice): <pure> list<jslex>
  match s.alexScan(0)
    AlexToken(s',len,p) ->
      Cons(p(s.subslice(0, len)), lex-recur(s'))
    AlexEOF ->
      Nil
    AlexSkip ->
      throw("Alex Skip?")
    AlexError(m) ->
      throw("Alex error" ++ m.string)

fun main()
  val s = r#"{"name":"Gpa142749579","age":365380512,"children":[{"name":"another child219059342"}],"adult-children":[]}"#
  s.parse.pretty.show.println