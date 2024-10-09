import json-lex-eff
import std/data/json
import std/pretty/pprint

type parse-state
  PList(l: ctx<list<json>>, ps': parse-state)
  PObjKey(l: ctx<list<(string, json)>>, ps': parse-state)
  PObjValue(l: ctx<list<(string, json)>>, s: string, ps': parse-state)
  Done(v: maybe<json>)

fun add-value(st: parse-state, js: json): exn parse-state
  match st
    PObjValue(l, s, ps) -> PObjKey(l ++ ctx Cons((s, js), _), ps)
    PList(l, ps) -> PList(l ++ ctx Cons(js, _), ps)
    Done -> Done(Just(js))
    _ -> throw("Invalid state for adding a value")

fun finish-value(st: parse-state): exn parse-state
  match st
    PList(l, ps) -> ps.add-value(JSList(l ++. Nil)) // Allows trailing commas
    PObjKey(l, ps) -> ps.add-value(JSObj(l ++. Nil)) // Allows trailing commas
    Done(s) -> Done(s)
    _ -> throw("Invalid state for finishing a value")

fun parse-json(str: string): pure json
  var object := Done(Nothing)
  var indent := 0
  with handler
    fun do-emit(j: json)
      trace(indent.spaces ++ "Emit " ++ j.show)
      object := object.add-value(j)
    fun start-object()
      trace(indent.spaces ++ "StartO")
      object := PObjKey(ctx _, object)
      indent := indent + 2
    fun start-array()
      trace(indent.spaces ++ "StartA")
      indent := indent + 2
      object := PList(ctx _, object)
    fun finish-object()
      indent := indent - 2
      trace(indent.spaces ++ "FinishO")
      object := object.finish-value
    fun finish-array()
      indent := indent - 2
      trace(indent.spaces ++ "FinishA")
      object := object.finish-value
    fun add-key(s: string)
      trace(indent.spaces ++ "Add key " ++ s)
      match object
        PObjKey(l, ps) -> object := PObjValue(l, s, ps)
    return(x)
      match object
        Done(Just(j)) -> j
  alex-scan(str.slice, 0)
  trace("Done")

fun main()
  val s = r#"{
    "name":"Gpa142749579",
    "age":-365380512, // Wow you are old
    "children": [{
        // This is a comment
       "name":"another child219059342", // Interesting name
     }],
    "adult-children":[
      // No adults here
    ], // Trailing comma
    // something here 
   }"#
  s.parse-json.pretty.show.println