import parsers/fixpoint-common
import parsing/fixpoint
import parsing/fix-pretty
import std/core-extras
import std/pretty/pprint
import std/data/linearmap
import std/data/json
import json-fixpoint

fun parse-stringchars(): parse<pvalue,sslice,<>> string
  parse-manychar(RAnyChar, _ != '"')

fun parse-string(): parse<pvalue,sslice,<>> string
  with memo(RString)
  pbracketed(RChar('"'), pdquote, RChar('"'), pdquote, RString, parse-stringchars)

fun parse-null(): parse<pvalue,sslice,<>> json
  with memo(RNull)
  parse-token("null")
  JSNull

fun parse-bool(): parse<pvalue,sslice,<>> json
  with memo(RBool)
  parse-choices([{parse-token("true"); JSBool(True)}, {parse-token("false"); JSBool(False)}])

fun pjson(): parse<pvalue,sslice,<>> json
  with memo(RJson)
  parse-choices([pobj, parse-array, parse-bool, parse-null, {JSInt(pint())}, {JSString(parse-string())}])

fun plist(): parse<pvalue,sslice,<>> list<json>
  parse-sep-by(RChar(','), parse-comma, RJson, pjson)

fun parse-array(): parse<pvalue,sslice,<>> json
  with memo(RArray)
  val vals:list<json> = pbracketed(RChar('['), popenbracket, RChar(']'), pclosebracket, RArray, plist)
  JSList(vals)

fun pfield(): parse<pvalue,sslice,<>> (string,json)
  val key = parse-string()
  parse-colon()
  val value = pjson()
  (key, value)

fun pfields(): parse<pvalue,sslice,<>> list<(string,json)>
  parse-sep-by(RChar(','), parse-comma, RKeyValue, pfield) 

fun pobj(): parse<pvalue,sslice,<>> json
  with memo(RObj)
  val fields: list<(string,json)> = pbracketed(RChar('{'), popencurly, RChar('}'), pclosecurly, RObj, pfields) 
  JSObj(fields)

fun ptop(): parse<pvalue,sslice,<>> json
  with memo(RTop)
  val res = pobj()
  peof()
  res

// TODO: Need to remove whitespace between tokens (not between "", and need manychar with escape character)
// TODO: Need to fix to use order2
fun main(): <pure,console> ()
  fixpoint/parse(r#"{"a":["b",true,false,null,{"c":[],"d":123}]}"#.slice, RTop, ptop).pretty.show.println