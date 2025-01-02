/*----------------------------------------------------------------------------
   Copyright 2024, Koka-Community Authors

   Licensed under the MIT License ("The License"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

import parsing/regex-parser
import std/data/linearmap
import std/text/regex
import std/num/float64

type jsValue
  JsNull
  JsBool(b: bool)
  JsInt(s: string, i:int = s.parse-int.default(0))
  JsDouble(s: string, d:float64 = s.parse-float64.default(0.0))
  JsString(s: string)
  JsArray(l: list<jsValue>)
  JsObject(o: jsObject<jsValue>)

alias jsObject<a> = linearMap<string, a>

fun toString(v: jsValue)
  match v
    JsString(s) -> s
    _ -> v.show

fun js/show(v: jsValue)
  match v
    JsNull -> "null"
    JsBool(b) -> if b then "true" else "false"
    JsInt(i) -> i.show
    JsDouble(d) -> d.show
    JsString(s) -> s.show
    JsArray(l) -> "[" ++ l.map(show).join(", ") ++ "]"
    JsObject(o) -> "{" ++ o.list.map(fn((k, v1)) k ++ ": " ++ v1.js/show).join(",\n ") ++ "}"

fun jsLookup(v: jsValue, keys: list<string>): jsValue
  match keys
    Nil -> v
    Cons(k, ks) ->
      match v
        JsObject(o) -> 
          match o.lookup(k)
            Just(v1) -> jsLookup(v1, ks)
            Nothing -> JsNull
        JsArray(vs) -> 
          match k.parse-int
            Just(i) -> 
              match vs[i]
                Just(v1) -> jsLookup(v1, ks)
                Nothing -> JsNull
            Nothing -> JsNull
        _ -> JsNull

fun jsFind(v: jsValue, def: jsValue, keys: list<string>): jsValue
  match v.jsLookup(keys)
    JsNull -> def
    v1 -> v1

val hexLeadRegex = r"0[xX]".regex();
fun hexadecimal()
  take-pattern(leadingRegex).string ++ take-pattern(hexLeadRegex).string ++ take-pattern(integerRegex).string

val leadingHexDotRegex = r"^.[0-9a-fA-F_]+([eEpP][+-]?[0-9a-fA-F_]+)?".regex() 
val wholeHexFloatRegex = r"^[0-9a-fA-F_]+(\.[0-9a-fA-F_]+)?([eEpP][+-]?[0-9a-fA-F_]+)?".regex() 
val integerRegex = r"^[0-9a-fA-F_]+".regex() 
fun integer()
  take-pattern(leadingRegex).string ++ take-pattern(integerRegex).string

val leadingRegex = r"[+-]".regex()
fun parse-exp()
  take-pattern(leadingRegex).string ++ integer()
  
fun parse-decfloat()
  take-pattern(leadingRegex).string ++ (fn() take-pattern(wholeHexFloatRegex).string).or({take-pattern(leadingHexDotRegex).string})

fun parse-hexfloat()
  take-pattern(leadingRegex).string ++ hexadecimal() ++ (fn() take-pattern(wholeHexFloatRegex).string).or({take-pattern(leadingHexDotRegex).string})

fun parse-literal()
  with scope("plit")
  ors([{text("true"); JsBool(True)},{text("false"); JsBool(False)},{text("null"); JsNull}])

fun parse-float()
  with atomic
  val d = or({parse-decfloat().split("_").join}, {parse-hexfloat().split("_").join})
  JsDouble(d)

fun parse-integer()
  with atomic
  val d = integer().split("_").join
  JsInt(d)

val stringRegex = "^\"[^\\\\\"]*(?:\\\\.[^\"\\\\]*)*\"".regex()

inline fun parse-str()
  take-pattern(stringRegex)

fun parse-string()
  with scope("parse-string")
  val s = parse-str()
  JsString(s.subslice(1, s.count - 1).string)

fun parse-array()
  with scope("parse-array")
  parse-token("[")
  val xs = parse-sep-trailing(parse-value,{parse-token(",")})
  optional(parse-whitespace)
  parse-token("]")
  JsArray(xs)

fun parse-member()
  with scope("pmember")
  val k = parse-str().string
  optional(parse-whitespace)
  parse-token(":")
  val v = parse-value()
  (k, v)

fun parse-object()
  with scope("parse-object")
  parse-token("{")
  val members = parse-sep-trailing(parse-member, {parse-token(",")})
  optional(parse-whitespace)
  val o = JsObject(LinearMap([]) + members)
  parse-token("}")
  o

fun parse-value(): <div,parse<string>> jsValue
  with scope("pvalue")
  val v = ors([parse-array,parse-object,parse-literal,parse-float,parse-integer,parse-string])
  optional(parse-whitespace)
  v

val spacesRegex = r"^([ \n\t\r]|\/\/[^\n]+)+".regex()
fun parse-whitespace()
  spacesRegex.take-pattern()

fun parse-token(s: string)
  text(s)
  optional(parse-whitespace)

fun parse(s: string)
  s.println
  match s.run(parse-value)
    Right(e) -> e.show.println
    Left(err) -> ("Error:\n" ++ err.show(s)).println
  
fun parse2(p: () -> parse<string> jsValue, s: string, rshow)
  s.println
  val x = match s.run(p)
    Right(e) -> e.rshow
    Left(err) -> "Error:\n" ++ err.show(s)
  x.println
  
fun main()
  parse(r#"{
  "hello":"world", // Something here
}"#
  )
  