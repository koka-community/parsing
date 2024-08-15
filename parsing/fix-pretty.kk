import parsing/fixpoint
import std/pretty/pprint
pub import std/data/linearmap
import std/data/linearset

fun cache/pretty(cache: parse-cache<c,l>, ?r/pretty: ruleid -> pure doc, ?l/pretty: l -> pure doc, ?c/pretty: c -> pure doc): pure doc
  val res = cache.list.map  fn((s, m)) 
    val m' = m.list.map fn((start, ms)) 
        if ms.list.is-empty then pprint/empty 
        else
          val res = ms.list.map fn((c,end)) c.c/pretty |+| "left:".text |+| end.l/pretty
          start.l/pretty |+| ":".pretty |+| res.tupled
    s.pretty |+| ":".pretty |--| m'.vcat.indent(2)
  res.vcat

fun slice/pretty(s: sslice): pure doc
  s.show.text