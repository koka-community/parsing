import parsers/fixpoint-common
import parsing/fixpoint
import parsing/fix-pretty
import std/pretty/pprint
import std/data/json
import std/core-extras

extend type ruleid
  RObj
  RString
  RArray
  RNumber
  RBool
  RNull
  RJson
  RKeyValue

extend type pvalue
  PJson(j: json)
  PJsonKeyValue(key: string, value: json)

fun ruleid/(==)(a: ruleid, b: ruleid): pure bool
  match (a, b)
    (RObj, RObj) -> True
    (RString, RString) -> True
    (RArray, RArray) -> True
    (RNumber, RNumber) -> True
    (RBool, RBool) -> True
    (RNull, RNull) -> True
    (RJson, RJson) -> True
    (RKeyValue, RKeyValue) -> True
    (RTop, RTop) -> True
    (REOF, REOF) -> True
    (RAnyChar, RAnyChar) -> True
    (RWhite, RWhite) -> True
    (RInt, RInt) -> True
    (RAlpha, RAlpha) -> True
    (RAlphaNum, RAlphaNum) -> True
    (RChar(c1), RChar(c2)) -> c1 == c2
    (RToken(s1), RToken(s2)) -> s1 == s2
    (RChars(k1), RChars(k2)) -> k1 == k2
    (RNot(k1), RNot(k2)) -> k1 == k2
    (RMaybe(k1), RMaybe(k2)) -> k1 == k2
    (RMany(r1, b1), RMany(r2, b2)) -> r1 == r2 && b1 == b2
    (RSepBy(r1, s1, b1), RSepBy(r2, s2, b2)) -> r1 == r2 && s1 == s2 && b1 == b2
    (RBracketed(l1, r1, b1), RBracketed(l2, r2, b2)) -> l1 == l2 && r1 == r2 && b1 == b2
    (RPrecededBy(r1, r2), RPrecededBy(r3, r4)) -> r1 == r3 && r2 == r4
    (RFollowedBy(r1, r2), RFollowedBy(r3, r4)) -> r1 == r3 && r2 == r4
    (RId(s1, r1), RId(s2, r2)) -> s1 == s2 && r1 == r2
    _ -> False

fun ruleid/show(a: ruleid): pure string
  match a
    RObj -> "Obj"
    RString -> "String"
    RArray -> "Array"
    RNumber -> "Number"
    RBool -> "Bool"
    RNull -> "Null"
    RJson -> "Json"
    RKeyValue -> "KeyValue"
    RTop -> "Top"
    REOF -> "EOF"
    RAnyChar -> "AnyChar"
    RWhite -> "White"
    RInt -> "Int"
    RAlpha -> "Alpha"
    RAlphaNum -> "AlphaNum"
    RChar(c) -> "Char(" ++ c.show ++ ")"
    RToken(s) -> "Token(" ++ s ++ ")"
    RChars(k) -> "Chars(" ++ k.show ++ ")"
    RNot(k) -> "Not(" ++ k.show ++ ")"
    RMaybe(k) -> "Maybe(" ++ k.show ++ ")"
    RMany(r, b) -> "Many(" ++ r.show ++ ", " ++ b.show ++ ")"
    RSepBy(r, s, b) -> "SepBy(" ++ r.show ++ ", " ++ s.show ++ ", " ++ b.show ++ ")"
    RBracketed(l, r, b) -> "Bracketed(" ++ l.show ++ ", " ++ r.show ++ ", " ++ b.show ++ ")"
    RPrecededBy(r1, r2) -> "PrecededBy(" ++ r1.show ++ ", " ++ r2.show ++ ")"
    RFollowedBy(r1, r2) -> "FollowedBy(" ++ r1.show ++ ", " ++ r2.show ++ ")"
    RId(s, r) -> "Id(" ++ s.show ++ ", " ++ r.show ++ ")"
    _ -> "other"

fun json/inj(j: json): pvalue
  PJson(j)

fun json/extract(c: pvalue): exn json
  match c
    PJson(j) -> j
    _ -> throw("Expected Json") 

fun jsonkv/inj((k: string, v: json)): pvalue
  PJsonKeyValue(k, v)

fun jsonkv/extract(c: pvalue): exn (string, json)
  match c
    PJsonKeyValue(k, v) -> (k, v)
    _ -> throw("Expected JsonKeyValue")


fun pvalue/show(a: pvalue): pure string
  match a
    PString(s) -> "String(" ++ s ++ ")"
    PList(l) -> "List(" ++ l.map(show).join(", ") ++ ")"
    PInt(i) -> "Int(" ++ i.show ++ ")"
    PEOF -> "EOF"
    PChar(c) -> "Char(" ++ c.show ++ ")"
    PJson(j) -> "Json(" ++ j.pretty.show ++ ")"
    PJsonKeyValue(key, value) -> "JsonKeyValue(" ++ key ++ ", " ++ value.pretty.show ++ ")"
    _ -> "other"

fun pvalue/(==)(a: pvalue, b: pvalue): pure bool
  match (a, b)
    (PChar(c1), PChar(c2)) -> c1 == c2
    (PString(s1), PString(s2)) -> s1 == s2
    (PList(l1), PList(l2)) -> eql/(==)(l1, l2)
    (PMaybe(Just(v1)), PMaybe(Just(v2))) -> v1 == v2
    (PMaybe(Nothing), PMaybe(Nothing)) -> True
    (PUnit, PUnit) -> True
    (PInt(i1), PInt(i2)) -> i1 == i2
    (PEOF, PEOF) -> True
    (PJson(j1), PJson(j2)) -> j1 == j2
    _ -> False

fun prule/pretty(r: ruleid): pure doc
  r.show.text

fun pvalue/pretty(v: pvalue): pure doc
  v.show.text
