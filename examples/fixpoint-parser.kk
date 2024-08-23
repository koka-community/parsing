import std/fixpoint/fixpoint-memo
import std/fixpoint/lattice
import std/data/rb-map
import std/core-extras

// Using fixpoint framework, but not fixpoint parser

type rule
  Top
  Char
  Last

fun rule/order2(r1: rule, r2: rule): order2<rule>
  match (r1, r2)
    (Top, Top) -> Eq2(Top)
    (Top, x) -> Gt2(x, Top)
    (x, Top) -> Lt2(x, Top)
    (Char, Char) -> Eq2(Char)
    (Char, x) -> Gt2(x, Char)
    (x, Char) -> Lt2(x, Char)
    (Last, Last) -> Eq2(Last)

fun sslice/order2(s1: sslice, s2: sslice): order2<sslice>
  if s1.count == s2.count then Eq2(s1)
  else if s1.count < s2.count then Lt2(s1, s2)
  else Gt2(s1, s2)

fun parse(s: sslice, r: rule)
  with each <- memo((s, r))
  match r
    Top ->
      if s.is-empty then none()
      else
        val (_, s') = parse(s, Char)
        each([{parse(s', Top)}, {parse(s', Last)}])
    Char ->
      match s.next()
        Nothing -> none()
        Just((c, s')) -> (c, s')
    Last ->
      match s.next()
        Nothing -> none()
        Just((c, s')) -> if s'.is-empty then (c, s') else none()

fun show(r: rule): string
  match r
    Top -> "Top"
    Char -> "Char"
    Last -> "Last"

fun main()
  val res : rbmap<(sslice, rule), list<(char, sslice)>> = 
    with cache
    parse("hello".slice, Top)
  res.show.println