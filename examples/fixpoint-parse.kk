import parsing/fixpoint
import std/data/linearmap
import std/data/linearset
import parsers/fixpoint-common
import std/core-extras
import std/pretty/pprint
import parsing/fix-pretty

extend type ruleid
  RFun

fun pvalue/(==)(c1: pvalue, c2: pvalue): pure bool
  match (c1, c2)
    (PChar(c1), PChar(c2)) -> c1 == c2
    (PList(l1), PList(l2)) -> eql/(==)(l1, l2)
    (PEOF, PEOF) -> True
    (PString(s1), PString(s2)) -> s1 == s2
    (PList(l1), PList(l2)) -> eql/(==)(l1, l2)
    _ -> False

fun pvalue/show(c: pvalue): pure string
  match c
    PChar(c) -> c.show
    PList(l) -> l.show
    PString(s) -> s
    PEOF -> "EOF"

fun pvalue/pretty(c: pvalue): pure doc
  c.show.text

fun ruleid/(==)(a: ruleid, b: ruleid): pure bool
  match (a, b)
    (RTop, RTop) -> True
    (RFun, RFun) -> True
    (REOF, REOF) -> True
    (RMany(a1,b1), RMany(a2,b2)) -> a1 == a2 && b1 == b2
    (RAnyChar, RAnyChar) -> True
    (RMaybe(a), RMaybe(b)) -> a == b
    (RToken(s1), RToken(s2)) -> s1 == s2
    _ -> False

fun ruleid/show(a: ruleid): pure string
  match a
    RTop -> "Top"
    RFun -> "Fun"
    REOF -> "EOF"
    RAnyChar -> "AnyChar"
    RMany(a,b) -> "Many(" ++ a.show ++ ", " ++ b.show ++ ")"
    RMaybe(a) -> "Maybe(" ++ a.show ++ ")"
    RToken(s) -> "Token(" ++ s ++ ")"
    _ -> "other"

fun ruleid/pretty(a: ruleid): pure doc
  a.show.text

fun pProgram(): parse<pvalue,sslice,e> string
  with memo(RTop)
  val res = pmany(RAnyChar, any/pchar)
  pEof()
  res.string

fun pProgram2(): parse<pvalue,sslice,e> list<string>
  with memo(RTop)
  val res = pmany(RFun, {ptoken("fun")})
  pEof()
  res

fun main()
  val res = 
    with parse-cache("funfunfun".slice)
    val res = pProgram2()
    trace(res.pretty.show)
    res
  res.pretty.show.println
