/*----------------------------------------------------------------------------
   Copyright 2024, Koka-Community Authors

   Licensed under the MIT License ("The License"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/

// This module provides a parser interface for parsing text
// It is based on the Unison parsing library https://share.unison-lang.org/@rlmark/parsing, but with some modifications for Koka.

import std/text/regex
import std/num/float64
import std/core/undiv

pub effect parse-fail<e>
  final ctl fail(e: perror<e>): a

pub alias parse<e> = <parse-try<e>,parse-fail<e>>

pub effect parse-try<e>
  fun try-text(s: string): either<perror<e>, ()>
  fun unmark(i: int): ()
  fun reset(i: int): ()
  fun try-pattern(p: regex): either<perror<e>, list<sslice>>
  fun try-skip(i: int): either<perror<e>, ()>
  fun try-take(i: int): either<perror<e>, string>
  fun offset(): int
  fun mark(): int

pub alias parser<r,a,e::E> = () -> <parse<r>|e> a

pub fun atomic(p: parser<r,a,e>): <parse<r>|e> a
  val n = mark()
  match try-parse(p)
    Left(e) ->
      reset(n)
      fail(e)
    Right(r) ->
      unmark(n)
      r

pub fun between(open: parser<r,a,e>, close: parser<r,a,e>, mid: parser<r,b,e>): <parse<r>|e> b
  productL({productR(open,mid)},close)

pub value type charQuery
  OneOf(text: string)
  Ranges(ranges: list<(char, char)>)

pub type perror<e>
  Expected(expected: string, offset: int)
  ExpectedPatternMatch(pat: string, offset: int)
  NotEnough(i: int, offset: int)
  In(label: string, offset: int, err: perror<e>)
  UserDefined(err: e, offset: int)
  ExpectedCharQuery(q: charQuery, offset: int)

pub fun failHere(e: (int -> perror<r>)): parse<r> a
  fail(e(offset()))

pub fun halve(first: parser<r,f,e>, separator: parser<r,s,e>, second: parser<r,s,e>): <parse<r>|e> (f, s)
  val a = first()
  separator()
  val b = second()
  (a, b)

pub fun literal(t: string): <parse<r>|e> string
  text(t)
  t

pub fun many(p: parser<r,a,e>): <parse<r>|e> list<a>
  match optional(p)
    Just(a) -> 
      val x = many(p.pretend-decreasing)
      Cons(a, x)
    Nothing -> []

pub fun many1(p: parser<r,a,<div|e>>): <parse<r>,div|e> list<a>
  val a = p()
  Cons(a, many(p))

pub fun optional(p: parser<r,a,e>): <parse<r>|e> maybe<a>
  or(fn() Just(p()), fn() Nothing)

pub fun or(p1: parser<r,a,e>, p2: parser<r,a,e>): <parse<r>|e> a
  val o1 = offset()
  match try-parse(p1)
    Left(e) ->
      if offset() == o1 then 
        p2() 
      else 
        fail(e)
    Right(r) -> r

pub fun ors(ps: list<parser<r,a,e>>): <parse<r>|e> a
  match ps
    Nil -> fail(Expected("List size > 1 for or parser", offset()))
    Cons(p1, Cons(p2, Nil)) -> or(p1, p2)
    Cons(p, pp) -> or(p, fn() ors(pp))

pub fun productL(a: parser<r,a,e>, b: parser<r,b,e>): <parse<r>|e> a
  match try-parse(a)
    Left(e) -> fail(e)
    Right(r) ->
      b()
      r

pub fun productR(a: parser<r,a,e>, b: parser<r,b,e>): <parse<r>|e> b
  match try-parse(a)
    Left(e) -> fail(e)
    Right(_) ->
      val vb = b()
      vb

pub fun reraise(p: parser<r,either<perror<r>,a>,e>): <parse<r>|e> a
  match p()
    Left(e) -> fail(e)
    Right(r) -> r

pub fun take-advance(s: sslice, i: int): (list<char>, sslice)
  if i == 0 then ([], s) else
    match s.next
      Nothing -> ([], s)
      Just((c,rest)) ->
        val (cs, newS) = take-advance(pretend-decreasing(rest), i - 1)
        (Cons(c, cs), newS)

pub fun starts-with(s: sslice, cs: list<char>): (bool, sslice, int)
  match cs
    Nil -> (True, s, 0)
    Cons(c', cs') ->
      match s.next
        Just((c, rest)) | c' == c ->
          val (r, re, i) = starts-with(rest.pretend-decreasing, cs'.pretend-decreasing)
          (r, re, i + 1)
        _ -> 
          (False, s, 0)

pub fun run(s: string, parser: parser<r,a,<exn|e>>): <exn|e> either<perror<r>, a>
  var offset := 0
  var rem := s.slice
  var marks := []
  with handler
    return(a) Right(a)
    final ctl fail(e)
      Left(e)
  handle(parser)
    fun try-take(i: int)
      if i > rem.count then Left(NotEnough(i, offset))
      else
        val (taken, rest) = rem.take-advance(i)
        rem := rest
        offset := offset + i
        Right(taken.string)
    fun try-skip(i: int)
      if i > rem.count then Left(NotEnough(i, offset))
      else
        rem := rem.advance(i)
        offset := offset + i
        Right(())
    fun try-text(t: string)
      val (v, rem', cnt) = rem.starts-with(t.list)
      if v then
        rem := rem'
        offset := offset + cnt
        Right(())
      else Left(Expected(t, offset))
    fun offset()
      offset
    fun try-pattern(pat: regex)
      match pat.exec(rem.string) // TODO: Remove when slice regex is supported
        Nil -> 
          Left(ExpectedPatternMatch(pat.source, offset))
        ms ->
          val m = ms[0].default("".slice)
          val c = m.count
          // trace("Got a match " ++ m.string ++ " length: " ++ c.show)
          rem := rem.advance(c)
          offset := offset + c
          Right(ms)
    fun mark()
      marks := marks ++ [(rem, offset)]
      marks.length - 1
    fun unmark(i: int)
      marks := marks.take(i)
      ()
    fun reset(i: int)
      match marks[i]
        Nothing -> throw("Invalid Mark: " ++ i.show)
        Just((rem', position')) ->
          marks := marks.take(i)
          rem := rem'
          offset := position'
          ()

pub fun scope(t: string, p: parser<r,a,e>): <parse<r>|e> a
  val off = offset()
  match try-parse(p)
    Left(e) -> fail(In(t, off, e))
    Right(r) -> r

pub fun sep1(s: parser<r,s,<div|e>>, p: parser<r,a,<div|e>>): <parse<r>,div|e> list<a>
  Cons(
    p(),
    many
      s()
      p()
  )

pub fun skip-until(q: charQuery): <parse<r>|e> ()
  val p = not-in-pattern(q)
  take-pattern(p.regex)
  ()

pub fun not-in-pattern(q: charQuery): string
  match q
    OneOf(t) -> "^[^" ++ t ++ "]"
    Ranges(chars) ->
      chars.foldl("^") fn(acc:string, v)
        val (c1:char, c2:char) = v
        val p = "[^" ++ c1.show ++ "-" ++ c2.show ++ "]"
        if acc.is-empty then p else acc ++ "|" ++ p

pub fun skip-while(q: charQuery): <parse<r>|e> ()
  //TODO: Encourage caching regexes for charQueries, or intern them?
  take-pattern(q.to-pattern.regex)
  ()

pub fun to-pattern(q: charQuery): string
  match q
    OneOf(t) -> "^[" ++ t ++ "]"
    Ranges(chars) ->
      chars.foldl("^") fn(acc:string, v)
        val (c1:char, c2:char) = v
        val p = "[" ++ c1.show ++ "-" ++ c2.show ++ "]"
        if acc.is-empty then p else acc ++ "|" ++ p

pub fun take-chars(i: int): <parse<r>|e> string
  reraise({try-take(i)})

pub fun take-pattern(p: regex): <parse<r>|e> sslice
  val res = reraise({try-pattern(p)})
  res[0].default("".slice)

pub fun take-patterns(p: regex): <parse<r>|e> list<sslice>
  reraise({try-pattern(p)})

pub fun take-until(q: charQuery): <parse<r>|e> sslice
  take-pattern(not-in-pattern(q).regex)

pub fun take-while(q: charQuery): <parse<r>|e> sslice
  take-pattern(q.to-pattern.regex)

pub fun take-while1(q: charQuery): <parse<r>|e> sslice
  val initial = offset()
  val res = take-pattern(q.to-pattern.regex)
  if offset() == initial then fail(ExpectedCharQuery(q, initial))
  else res

pub fun take-whitespace(): <parse<r>|e> sslice
  take-while(OneOf(" \t\r"))

pub fun text(t: string): <parse<r>|e> ()
  reraise({try-text(t)})

pub fun str/token(t: string): <parse<r>|e> string
  text(t)
  t

pub fun parser/token(p: parser<r,a,e>): <parse<r>|e> a
  val res = p()
  skip-whitespace()
  res

pub fun tokenize(p: parser<r,a,e>): <parse<r>,div|e> list<sslice>
  sep1(skip-whitespace, take-non-whitespace)

pub fun try-parse(p: parser<r,a,e>): <parse<r>|e> either<perror<r>,a>
  with override
    return(a) Right(a)
    final ctl fail(e) Left(e)
  p()

pub fun perr/show(e: perror<e>, input: string, indent:string="", ?show: (e) -> string): string
  match e
    ExpectedCharQuery(q, offset) -> indent ++ "Expected " ++ q.show ++ " at " ++ offset.show ++ " but found " ++ show-line-offset(input, offset)
    ExpectedPatternMatch(pat, offset) -> indent ++ "Expected pattern " ++ pat ++ " at " ++ offset.show ++ " but found " ++ show-line-offset(input, offset)
    Expected(t, offset) -> indent ++ "Expected " ++ t ++ " at " ++ offset.show ++ " but found " ++ show-line-offset(input, offset)
    NotEnough(i, offset) -> indent ++ "Not enough input at " ++ offset.show ++ ", expected " ++ i.show ++ " but found " ++ show-line-offset(input, offset)
    In(label, offset, err) -> indent ++ "In " ++ label ++ " at position " ++ offset.show ++ " found " ++ show-line-offset(input, offset) ++ "\n" ++ show(err, input, indent ++ "  ")
    UserDefined(err, offset) -> indent ++ show(err) ++ " but found " ++ show-line-offset(input, offset)

fun show-line-offset(s: string, offset: int): string
  val sub = s.slice.advance(offset)
  sub.advance(-10).subslice(0, min(offset, 10)).string ++ "(->" ++ sub.subslice(0,1).string ++ "<-)" ++ sub.subslice(1, 10).string

pub fun cq/show(q: charQuery): string
  match q
    OneOf(t) -> "one of " ++ t
    Ranges(chars) ->
      chars.foldl("") fn(acc:string, v)
        val (c1:char, c2:char) = v
        if acc.is-empty then c1.show ++ "-" ++ c2.show
        else acc ++ ", " ++ c1.show ++ "-" ++ c2.show

// Common
val leadingDotRegex = r"^[+-]?\.[0-9]+([eE][+-]?[0-9]+)?".regex() 
val wholeFloatRegex = r"^[+-]?[0-9]+(\.[0-9]+)?([eE][+-]?[0-9]+)?".regex() 
val plusOrMinusRegex = r"^[+-]?".regex()

pub fun parse-float(): <parse<r>|e> float64
  val pos = offset()
  val result = parse-leading-dot-float.or(parse-whole-float)
  match parse-float64(result.string)
    Just(i) -> i
    Nothing -> failHere(fn(p) Expected("parsable float " ++ result.string, pos))

fun parse-whole-float(): <parse<r>|e> sslice
  take-pattern(wholeFloatRegex)

fun parse-leading-dot-float(): <parse<r>|e> sslice
  take-pattern(leadingDotRegex)

val wholeNumRegex = r"^[-+]?[0-9]+".regex() 
fun parse-int(): <parse<r>|e> int
  match try-parse({take-pattern(wholeNumRegex)})
    Left(e) -> fail(e)
    Right(x) -> 
      val pos = offset()
      match parse-int(x.string)
        Just(i) -> i
        Nothing -> failHere(fn(p) Expected("int", pos))


val alphaRegex = "^[a-zA-Z]".regex()
pub fun take-alpha(): <parse<r>|e> sslice
  take-pattern(alphaRegex)

val alnumRegex = "^[a-zA-Z0-9]+".regex()
pub fun take-alphanum(): <parse<r>|e> sslice
  take-pattern(alnumRegex)

pub fun take-alphanum1(): <parse<r>|e> sslice
  take-pattern(alnumRegex)

pub fun skip-whitespace(): <parse<r>|e> ()
  skip-while(OneOf(" \t\r\n"))

pub fun parse-slash(): <parse<r>|e> ()
  text("/")

pub fun parse-space(): <parse<r>|e> ()
  text(" ")

pub fun take-non-whitespace(): <parse<r>|e> sslice
  take-while1(Ranges([('!', '~')]))

pub fun parse-open-paren(): <parse<r>|e> ()
  text("(")

pub fun parse-close-paren(): parse<e> ()
  text(")")

pub fun parse-colon(): parse<e> ()
  text(":")

pub fun parse-comma(): parse<e> ()
  text(",")

pub fun parse-clrf(): parse<e> ()
  text("\r\n")

pub fun parse-newline(): <parse<r>|e> ()
  text("\n")

pub fun parse-end-of-line(): parse<e> ()
  parse-newline.or(parse-clrf)

pub fun parse-end-of-input(): parse<e> ()
  with atomic
  val posMarker = offset()
  match try-take(1)
    Left(_) -> ()
    Right(_) -> fail(Expected("End of string", posMarker))

pub fun parse-sep-trailing(p: parser<r,a,<div|e>>, sep: parser<r,b,<div|e>>): <div,parse<r>|e> list<a>
  val x = optional(p)
  match x
    Just(x1) -> 
      val xs = many({with atomic; sep(); p()})
      optional(sep)
      Cons(x1, xs)
    Nothing -> []