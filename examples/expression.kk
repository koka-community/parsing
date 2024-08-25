import parsers/fixpoint-common
import parsing/fixpoint
import parsing/fix-pretty
import std/core-extras
import std/pretty/pprint
import std/data/linearmap
import expression-helper

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
  parse-cache("1+3*4+1".slice, RTop, ptop).pretty.show.println