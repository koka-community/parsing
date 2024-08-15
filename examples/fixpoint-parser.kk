import std/fixpoint/fixpoint-memo
import std/fixpoint/lattice
import std/data/linearmap

type rule
  Top
  Char
  Last

fun rule/(==)(r1: rule, r2: rule): bool
  match (r1, r2)
    (Top, Top) -> True
    (Char, Char) -> True
    (Last, Last) -> True
    _ -> False

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
  val res : linearMap<(sslice, rule), list<(char, sslice)>> = 
    with cache
    parse("hello".slice, Top)
  res.show.println
