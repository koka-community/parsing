import alex
import parsers/alex/json-parse
import std/data/json
import std/num/float64
import std/pretty/pprint

type parse-state
  PList(l: ctx<list<json>>, ps': parse-state)
  PListCommaEnd(l: ctx<list<json>>, ps': parse-state)
  PObjKey(l: ctx<list<(string, json)>>, ps': parse-state)
  PObjColon(l: ctx<list<(string, json)>>, s: string, ps': parse-state)
  PObjValue(l: ctx<list<(string, json)>>, s: string, ps': parse-state)
  PObjCommaEnd(l: ctx<list<(string, json)>>, ps': parse-state)
  Done(v: maybe<json>)

fun add-value(st: parse-state, js: json): exn parse-state
  match st
    PObjValue(l, s, ps) -> PObjCommaEnd(l ++ ctx Cons((s, js), _), ps)
    PList(l, ps) -> PListCommaEnd(l ++ ctx Cons(js, _), ps)
    PListCommaEnd(l, ps) -> ps.add-value(JSList(l ++. Cons(js, Nil)))
    Done -> Done(Just(js))
    _ -> throw("Invalid state for adding a value")

fun finish-value(st: parse-state): exn parse-state
  match st
    PList(l, ps) -> ps.add-value(JSList(l ++. Nil)) // Allows trailing commas
    PListCommaEnd(l, ps) -> ps.add-value(JSList(l ++. Nil)) 
    PObjCommaEnd(l, ps) -> ps.add-value(JSObj(l ++. Nil)) 
    PObjKey(l, ps) -> ps.add-value(JSObj(l ++. Nil)) // Allows trailing commas
    Done(s) -> Done(s)
    _ -> throw("Invalid state for finishing a value")

fun add-str(st: parse-state, s:sslice): exn parse-state
  val s' = s.string
  match st
    PObjKey(l, ps) -> PObjColon(l, s', ps)
    _ -> st.add-value(JSString(s'))

fun add-num(st: parse-state, s: sslice): exn parse-state
  val s' = s.string
  match parse-int(s') 
    Just(i) -> st.add-value(JSInt(i))
    Nothing -> 
      match parse-float64(s')
        Just(f) -> st.add-value(json/JSNum(f))
        _ -> throw("Invalid int " ++ s')

fun recur/parse(st: parse-state, s: sslice): <alextrace,pure> json
  match s.alexScan(0)
    AlexToken(s',len,p) ->
      match p(s.subslice(0, len))
        JSWhite -> parse(st, s')
        JSComment -> parse(st, s')
        JSArrayOpen -> parse(PList(ctx _, st), s')
        JSObjOpen -> parse(PObjKey(ctx _, st), s')
        JSArrayClose -> parse(st.finish-value, s')
        JSObjClose -> parse(st.finish-value, s')
        JSStr(str) -> parse(st.add-str(str), s')
        JSTrue -> parse(st.add-value(JSBool(True)), s')
        JSFalse -> parse(st.add-value(JSBool(False)), s')
        json-parse/JSNum(str) -> parse(st.add-num(str), s')
        json-parse/JSNull -> parse(st.add-value(JSNull), s')
        JSValueSep ->
          match st
            PObjCommaEnd(l, ps) -> parse(PObjKey(l, ps), s')
            PListCommaEnd(l, ps) -> parse(PList(l, ps), s')
            _ -> throw("Unexpected comma")
        JSObjColon ->
          match st
            PObjColon(l, str, ps) -> parse(PObjValue(l, str, ps), s')
            _ -> throw("Unexpected colon")
    AlexEOF ->
      match st.finish-value
        Done(Just(js)) -> js
        _ -> throw("Unexpected EOF")
    AlexSkip ->
      throw("Alex Skip?")
    AlexError(m) ->
      throw("Alex error" ++ m.string)

fun json/parse(s: string): pure json
  with val alextrace = False
  recur/parse(Done(Nothing), s.slice)

fun main()
  val s = r#"{
    "name":"Gpa142749579",
    "age":365380512, // Wow you are old
    "children": [{
        // This is a comment
       "name":"another child219059342", // Interesting name
     }],
    "adult-children":[
      // No adults here
    ], // Trailing comma
    // something here 
   }"#
  s.parse.pretty.show.println