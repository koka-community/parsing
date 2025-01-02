import parsing/fixpoint
import std/data/rb-map
import std/data/rb-set
import parsers/fixpoint-common
import std/pretty/pprint
import parsing/fix-pretty
import std/core-extras
pub import fixpoint-parse-helper

fun ruleid/pretty(a: ruleid): pure doc
  a.show.text

fun parse-program(): parse<pvalue,sslice,e> string
  with memo(RTop)
  val res = parse-many(RAnyChar, any/pchar)
  pEof()
  res.string

fun parse-program2(): parse<pvalue,sslice,e> list<string>
  with memo(RTop)
  val res = parse-many(RFun, {parse-token("fun")})
  pEof()
  res

fun main()
  val res = 
    with parse-cache("funfunfun".slice, RTop)
    val res = parse-program2()
    trace(res.pretty.show)
    res
  res.pretty.show.println
