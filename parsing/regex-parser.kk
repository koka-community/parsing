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
  fun tryText(s: string): either<perror<e>, ()>
  fun unmark(i: int): ()
  fun reset(i: int): ()
  fun tryPattern(p: regex): either<perror<e>, list<sslice>>
  fun trySkip(i: int): either<perror<e>, ()>
  fun tryTake(i: int): either<perror<e>, string>
  fun offset(): int
  fun mark(): int

pub alias parser<r,a,e::E> = () -> <parse<r>|e> a

pub fun atomic(p: parser<r,a,e>): <parse<r>|e> a
  val n = mark()
  match tryParse(p)
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
  match tryParse(p1)
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
  match tryParse(a)
    Left(e) -> fail(e)
    Right(r) ->
      b()
      r

pub fun productR(a: parser<r,a,e>, b: parser<r,b,e>): <parse<r>|e> b
  match tryParse(a)
    Left(e) -> fail(e)
    Right(_) ->
      val vb = b()
      vb

pub fun reraise(p: parser<r,either<perror<r>,a>,e>): <parse<r>|e> a
  match p()
    Left(e) -> fail(e)
    Right(r) -> r

pub fun takeAdvance(s: sslice, i: int): (list<char>, sslice)
  if i == 0 then ([], s) else
    match s.next
      Nothing -> ([], s)
      Just((c,rest)) ->
        val (cs, newS) = takeAdvance(pretend-decreasing(rest), i - 1)
        (Cons(c, cs), newS)

pub fun startsWith(s: sslice, cs: list<char>): (bool, sslice, int)
  match cs
    Nil -> (True, s, 0)
    Cons(c', cs') ->
      match s.next
        Just((c, rest)) | c' == c ->
          val (r, re, i) = startsWith(rest.pretend-decreasing, cs'.pretend-decreasing)
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
    fun tryTake(i: int)
      if i > rem.count then Left(NotEnough(i, offset))
      else
        val (taken, rest) = rem.takeAdvance(i)
        rem := rest
        offset := offset + i
        Right(taken.string)
    fun trySkip(i: int)
      if i > rem.count then Left(NotEnough(i, offset))
      else
        rem := rem.advance(i)
        offset := offset + i
        Right(())
    fun tryText(t: string)
      val (v, rem', cnt) = rem.startsWith(t.list)
      if v then
        rem := rem'
        offset := offset + cnt
        Right(())
      else Left(Expected(t, offset))
    fun offset()
      offset
    fun tryPattern(pat: regex)
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
  match tryParse(p)
    Left(e) -> fail(In(t, off, e))
    Right(r) -> r

pub fun sep1(s: parser<r,s,<div|e>>, p: parser<r,a,<div|e>>): <parse<r>,div|e> list<a>
  Cons(
    p(),
    many
      s()
      p()
  )

pub fun skipUntil(q: charQuery): <parse<r>|e> ()
  val p = notInPattern(q)
  takePattern(p.regex)
  ()

pub fun notInPattern(q: charQuery): string
  match q
    OneOf(t) -> "^[^" ++ t ++ "]"
    Ranges(chars) ->
      chars.foldl("^") fn(acc:string, v)
        val (c1:char, c2:char) = v
        val p = "[^" ++ c1.show ++ "-" ++ c2.show ++ "]"
        if acc.is-empty then p else acc ++ "|" ++ p

pub fun skipWhile(q: charQuery): <parse<r>|e> ()
  //TODO: Encourage caching regexes for charQueries, or intern them?
  takePattern(q.toPattern.regex)
  ()

pub fun toPattern(q: charQuery): string
  match q
    OneOf(t) -> "^[" ++ t ++ "]"
    Ranges(chars) ->
      chars.foldl("^") fn(acc:string, v)
        val (c1:char, c2:char) = v
        val p = "[" ++ c1.show ++ "-" ++ c2.show ++ "]"
        if acc.is-empty then p else acc ++ "|" ++ p

pub fun takeChars(i: int): <parse<r>|e> string
  reraise({tryTake(i)})

pub fun takePattern(p: regex): <parse<r>|e> sslice
  val res = reraise({tryPattern(p)})
  res[0].default("".slice)

pub fun takePatterns(p: regex): <parse<r>|e> list<sslice>
  reraise({tryPattern(p)})

pub fun takeUntil(q: charQuery): <parse<r>|e> sslice
  takePattern(notInPattern(q).regex)

pub fun takeWhile(q: charQuery): <parse<r>|e> sslice
  takePattern(q.toPattern.regex)

pub fun takeWhile1(q: charQuery): <parse<r>|e> sslice
  val initial = offset()
  val res = takePattern(q.toPattern.regex)
  if offset() == initial then fail(ExpectedCharQuery(q, initial))
  else res

pub fun takeWhitespace(): <parse<r>|e> sslice
  takeWhile(OneOf(" \t\r"))

pub fun text(t: string): <parse<r>|e> ()
  reraise({tryText(t)})

pub fun str/token(t: string): <parse<r>|e> string
  text(t)
  t

pub fun parser/token(p: parser<r,a,e>): <parse<r>|e> a
  val res = p()
  skipWhitespace()
  res

pub fun tokenize(p: parser<r,a,e>): <parse<r>,div|e> list<sslice>
  sep1(skipWhitespace, nonWhitespace)

pub fun tryParse(p: parser<r,a,e>): <parse<r>|e> either<perror<r>,a>
  with override
    return(a) Right(a)
    final ctl fail(e) Left(e)
  p()

pub fun perr/show(e: perror<e>, input: string, indent:string="", ?show: (e) -> string): string
  match e
    ExpectedCharQuery(q, offset) -> indent ++ "Expected " ++ q.show ++ " at " ++ offset.show ++ " but found " ++ showLineOffset(input, offset)
    ExpectedPatternMatch(pat, offset) -> indent ++ "Expected pattern " ++ pat ++ " at " ++ offset.show ++ " but found " ++ showLineOffset(input, offset)
    Expected(t, offset) -> indent ++ "Expected " ++ t ++ " at " ++ offset.show ++ " but found " ++ showLineOffset(input, offset)
    NotEnough(i, offset) -> indent ++ "Not enough input at " ++ offset.show ++ ", expected " ++ i.show ++ " but found " ++ showLineOffset(input, offset)
    In(label, offset, err) -> indent ++ "In " ++ label ++ " at position " ++ offset.show ++ " found " ++ showLineOffset(input, offset) ++ "\n" ++ show(err, input, indent ++ "  ")
    UserDefined(err, offset) -> indent ++ show(err) ++ " but found " ++ showLineOffset(input, offset)

fun showLineOffset(s: string, offset: int): string
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

pub fun parseFloat(): <parse<r>|e> float64
  val pos = offset()
  val result = parseLeadingDotFloat.or(parseWholeFloat)
  match parse-float64(result.string)
    Just(i) -> i
    Nothing -> failHere(fn(p) Expected("parsable float " ++ result.string, pos))

fun parseWholeFloat(): <parse<r>|e> sslice
  takePattern(wholeFloatRegex)

fun parseLeadingDotFloat(): <parse<r>|e> sslice
  takePattern(leadingDotRegex)

val wholeNumRegex = r"^[-+]?[0-9]+".regex() 
fun parseInt(): <parse<r>|e> int
  match tryParse({takePattern(wholeNumRegex)})
    Left(e) -> fail(e)
    Right(x) -> 
      val pos = offset()
      match parse-int(x.string)
        Just(i) -> i
        Nothing -> failHere(fn(p) Expected("int", pos))


val alphaRegex = "^[a-zA-Z]".regex()
pub fun takeAlpha(): <parse<r>|e> sslice
  takePattern(alphaRegex)

val alnumRegex = "^[a-zA-Z0-9]+".regex()
pub fun takeAlphanum(): <parse<r>|e> sslice
  takePattern(alnumRegex)

pub fun takeAlphanum1(): <parse<r>|e> sslice
  takePattern(alnumRegex)

pub fun skipWhitespace(): <parse<r>|e> ()
  skipWhile(OneOf(" \t\r\n"))

pub fun parseSlash(): <parse<r>|e> ()
  text("/")

pub fun parseSpace(): <parse<r>|e> ()
  text(" ")

pub fun nonWhitespace(): <parse<r>|e> sslice
  takeWhile1(Ranges([('!', '~')]))

pub fun parseOpenParen(): <parse<r>|e> ()
  text("(")

pub fun parseCloseParen(): parse<e> ()
  text(")")

pub fun parseColon(): parse<e> ()
  text(":")

pub fun parseComma(): parse<e> ()
  text(",")

pub fun parseCrlf(): parse<e> ()
  text("\r\n")

pub fun parseNewline(): <parse<r>|e> ()
  text("\n")

pub fun parseEndOfLine(): parse<e> ()
  parseNewline.or(parseCrlf)

pub fun parseEndOfInput(): parse<e> ()
  with atomic
  val posMarker = offset()
  match tryTake(1)
    Left(_) -> ()
    Right(_) -> fail(Expected("End of string", posMarker))

pub fun separatedTrailing(p: parser<r,a,<div|e>>, sep: parser<r,b,<div|e>>): <div,parse<r>|e> list<a>
  val x = optional(p)
  match x
    Just(x1) -> 
      val xs = many({with atomic; sep(); p()})
      optional(sep)
      Cons(x1, xs)
    Nothing -> []