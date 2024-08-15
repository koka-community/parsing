import parsers/fixpoint-common
import parsing/fixpoint
import parsing/fix-pretty
import std/core-extras
import std/pretty/pprint
import std/data/linearmap

extend type ruleid
  RExpr(prec: int)
  RRExpr(prec: int, pv: pvalue)

extend type pvalue
  PApp(f: string, args: list<pvalue>)

fun ruleid/(==)(a: ruleid, b: ruleid): pure bool
  match (a, b)
    (RExpr(p1), RExpr(p2)) -> p1 == p2
    (RRExpr(p1, pv1), RRExpr(p2, pv2)) -> p1 == p2 && pv1 == pv2
    (RChar(c1), RChar(c2)) -> c1 == c2
    (RInt, RInt) -> True
    (RTop, RTop) -> True
    (REOF, REOF) -> True
    (RToken(s1), RToken(s2)) -> s1 == s2
    _ -> False

fun ruleid/show(a: ruleid): pure string
  match a
    RExpr(p) -> "Expr " ++ p.show
    RRExpr(p, pv) -> "RExpr " ++ p.show ++ " " ++ pv.show
    RInt -> "Int"
    RTop -> "Top"
    REOF -> "EOF"
    RToken(s) -> "Token(" ++ s ++ ")"
    RChar(c) -> "Char(" ++ c.show ++ ")"
    _ -> "other"

fun pvalue/show(a: pvalue): pure string
  match a
    PString(s) -> "String(" ++ s ++ ")"
    PList(l) -> "List(" ++ l.map(show).join(", ") ++ ")"
    PInt(i) -> "Int(" ++ i.show ++ ")"
    PEOF -> "EOF"
    PApp(f, args) -> f ++ "(" ++ args.map(show).join(", ") ++ ")"
    PChar(c) -> "Char(" ++ c.show ++ ")"
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
    (PApp(f1, args1), PApp(f2, args2)) -> f1 == f2 && eql/(==)(args1, args2)
    _ -> False

fun prule/pretty(r: ruleid): pure doc
  r.show.text

fun pvalue/pretty(v: pvalue): pure doc
  v.show.text

fun pexpr(prec: int): parse<pvalue,sslice,<>> pvalue
  with memo(RExpr(prec))
  val left = PInt(pint())
  pexprr(prec, left)

fun pexprr(prec: int, left: pvalue): parse<pvalue,sslice,<>> pvalue
  with memo(RRExpr(prec, left))
  choices([
    associate(prec, AssociateRight, 3, left, {pchar('^').string}), 
    associate(prec, AssociateLeft, 2, left, {pchar('/').string}), 
    associate(prec, AssociateLeft, 2, left, {pchar('*').string}),  
    associate(prec, AssociateLeft, 1, left, {pchar('-').string}),
    associate(prec, AssociateLeft, 1, left, {pchar('+').string}), 
    {left}
  ])

fun associate(prec: int, a: associativity, opprec: int, pv: pvalue, op: parser<pvalue,sslice,string,<>>): div parser<pvalue,sslice,pvalue,<>>
  fn()
    if prec < opprec then
      val o = op()
      val newprec = match a
        AssociateLeft -> opprec
        AssociateRight -> opprec - 1
        AssociateNone -> prec
      PApp(o, [pv, pexprr(prec, pexpr(newprec))])
    else none()

type associativity
  AssociateLeft
  AssociateRight
  AssociateNone

fun ptop(): parse<pvalue,sslice,<>> pvalue
  with memo(RTop)
  val res = pexpr(0)
  peof()
  res

fun main(): <pure,console> ()
  parse-cache("1+3*4+1".slice, ptop).pretty.show.println