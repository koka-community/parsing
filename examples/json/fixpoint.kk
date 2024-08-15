import parsers/fixpoint-common
import parsing/fixpoint
import parsing/fix-pretty
import std/core-extras
import std/pretty/pprint
import std/data/linearmap
import std/data/json
import json-fixpoint

fun pstringchars(): parse<pvalue,sslice,<>> string
  pmanychar(RAnyChar, fn(c) c != '"')

fun pstring(): parse<pvalue,sslice,<>> string
  with memo(RString)
  pbracketed(RChar('"'), pdquote, RChar('"'), pdquote, RString, pstringchars)

fun pnull(): parse<pvalue,sslice,<>> json
  with memo(RNull)
  ptoken("null")
  JSNull

fun pbool(): parse<pvalue,sslice,<>> json
  with memo(RBool)
  choices([{ptoken("true"); JSBool(True)}, {ptoken("false"); JSBool(False)}])

fun pjson(): parse<pvalue,sslice,<>> json
  with memo(RJson)
  choices([pobj, parray, pbool, pnull, {JSInt(pint())}, {JSString(pstring())}])

fun plist(): parse<pvalue,sslice,<>> list<json>
  psepby(RChar(','), pcomma, RJson, pjson)

fun parray(): parse<pvalue,sslice,<>> json
  with memo(RArray)
  val vals:list<json> = pbracketed(RChar('['), popenbracket, RChar(']'), pclosebracket, RArray, plist)
  JSList(vals)

fun pfield(): parse<pvalue,sslice,<>> (string,json)
  val key = pstring()
  pcolon()
  val value = pjson()
  (key, value)

fun pfields(): parse<pvalue,sslice,<>> list<(string,json)>
  psepby(RChar(','), pcomma, RKeyValue, pfield) 

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
fun main(): <pure,console> ()
  parse(r#"{"a":["b",true,false,null,{"c":[],"d":123}]}"#.slice, RTop, ptop).pretty.show.println