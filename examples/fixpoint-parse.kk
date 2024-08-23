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
    with parse-cache("funfunfun".slice, RTop)
    val res = pProgram2()
    trace(res.pretty.show)
    res
  res.pretty.show.println
