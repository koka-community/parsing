import alex
import parsers/alex/json-parse
import parsing/ast-simple
import std/data/json
import std/data/linearmap
import std/num/float64
import std/pretty/pprint

fun pWhite()
  pmany("Whitespace")
    ptoken("Whitespace Char") fn(t)
      match t
        JSWhite -> Just(())
        _ -> Nothing

fun trailingWhite(f: () -> ast<jslex> a): ast<jslex> a
  val x = f()
  pWhite()
  x

fun pJsonValue()
  trailingWhite
    pchoices("JSValue", [pArray, pObject, pNull, pBool, pNum, pString])

fun is-token(nm: string, b: (jslex) -> bool): ast<jslex> jslex
  trailingWhite 
    ptoken(nm) fn(t: jslex)
      if t.b then Just(t) else Nothing

fun map-token(nm: string, b: (jslex) -> bool, m: (jslex) -> pure a): ast<jslex> a
  trailingWhite
    ptoken(nm) fn(t)
      if t.b then Just(m(t)) else Nothing

fun pNull()
  with _ <- map-token("Null", json-parse/is-jsnull)
  json/JSNull

fun pBool()
  with b <- map-token("Bool", fn(t) t.is-jstrue || t.is-jsfalse)
  match b
    JSTrue -> JSBool(True)
    JSFalse -> JSBool(False)

fun pAOpen()
  is-token("List start", is-jsarrayOpen)

fun pAClose()
  is-token("List end", is-jsarrayClose)

fun pOOpen()
  is-token("Object start", is-jsobjOpen)

fun pOClose()
  is-token("Object end", is-jsobjClose)

fun pComma()
  is-token(",", is-jsvalueSep)

fun pColon()
  is-token(":", is-jsobjColon)

fun pStr()
  map-token("string", is-jsstr, fn(JSStr(s)) s.string)

fun pNum()
  with n <- map-token("num", json-parse/is-jsnum)
  val json-parse/JSNum(s) = n
  val s' = s.string
  match parse-int(s') 
    Just(i) -> JSInt(i)
    Nothing -> 
      match parse-float64(s')
        Just(f) -> json/JSNum(f)
        _ -> throw("Invalid int " ++ s')

fun pString()
  JSString(pStr())

fun pArray()
  trailingWhite
    p-braced(pAOpen, pAClose)
      JSList(p-sep-by("Array Entry", pJsonValue, pComma))

fun pObject()
  trailingWhite
    p-braced(pOOpen, pOClose) 
      JSObj(p-sep-by("Obj Entry", pObjEntry, pComma))

fun pObjEntry()
  val nm = pStr()
  pColon()
  (nm, pJsonValue())

fun json/parse(s: string): <console,pure> json
  with final ctl astError(e)
    e.throw
  lexParse(s, pJsonValue, ?lex=lex)

fun lex(s: string): pure list<jslex>
  with val alextrace = False
  lex-recur(s.slice)

fun lex-recur(s: sslice): <alextrace,pure> list<jslex>
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