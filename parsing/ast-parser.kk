/*----------------------------------------------------------------------------
   Copyright 2024, Koka-Community Authors

   Licensed under the MIT License ("The License"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/
module ast-parser

import tokens/range

effect astError
  final ctl astError(e: string, r: range): a

effect astParse<l>
  val sourceInfo: source
  fun addWarning(w: string, r: range): ()
  fun ppeek(): maybe<l>
  fun undelimit(msg: string): ()
  fun reset(msg: string): ()
  fun delimit(msg: string): ()
  fun pnext(): maybe<l>
  fun peof(): ()
  fun tookInput(msg: string): bool
  fun ptrace(s: string): ()
  fun takeAll(): list<l>
  fun putAll(lexemes: list<l>): ()

fun pdelimit(msg: string, f: () -> <astParse<l>,astError|e> a): <astParse<l>,astError|e> a
  delimit(msg)
  with override
    final ctl astError(e, r) 
      undelimit(msg)
      astError("\nWhen parsing " ++ msg ++ " " ++ e, r)
  val x = f()
  undelimit(msg)
  x

fun peek(): ast<l> l
  val p = ppeek()
  match p
    Just(l) -> l
    Nothing -> astError("unexpected end of file", null)

fun next(): ast<l> l
  val p = pnext()
  match p
    Just(l) -> l
    Nothing -> astError("unexpected end of file", null)

alias ast<l> = <astParse<l>, astError, pure>
fun lexParse(p: () -> <console,ast<l>|e> a, sourceName: string, input: string, preprocess: list<l> -> <console, pure, astError|e> list<l> = id, ?lex: (source, int, string) -> <console, pure, astError|e> list<l>, ?show: (l) -> div string, ?range: (l) -> range): <console,astError,pure|e> a
  val src = Source(sourceName, input)
  val lexes = lex(src, 1, input)
  val lexes' = preprocess(lexes)
  parseLexemes(src, lexes', p)
  
fun parseLexemes(src: source, lexemes0: list<l>, p: () -> <console,ast<l>|e> a, ?show: (l) -> div string, ?range: (l) -> range): <console,astError,pure|e> a
  var lexemes := lexemes0
  var history := Cons((ctx _, "top", 0), Nil)
  val debug = True
  with override
    final ctl astError(e: string, r: range)
      astError("Got error: " ++ e ++ "\n" ++ " when parsing:\n\t" ++ lexemes.take(2).map(show).join("\n\t"), r)
  with handler
    val sourceInfo = src
    fun takeAll()
      val l = lexemes
      lexemes := Nil // TODO: This ruins the history -- especially with delimiting
      // history := Cons((ctx _, "top", 0), Nil)
      l
    fun putAll(lexemes')
      lexemes := lexemes' // TODO: This ruins the history -- especially with delimiting
      // history := Cons((ctx _, "top", 0), Nil)
    fun addWarning(w: string, r: range)
      ()
    fun pnext()
      match lexemes
        Cons(l, rst) ->
          lexemes := rst 
          match history
            Cons((h, msg, i), tl) ->
              history := Cons((h ++ ctx Cons(l, _), msg, i + 1), tl)
            Nil -> 
              astError("unexpected top of context", null)
          Just(l)
        Nil -> Nothing
    fun reset(msg)
      match history
        Cons((h, str, _), rst) -> 
          if str != msg then
            trace("Resetting " ++ str ++ " but expected " ++ msg)
            throw("Reset error")
          else
            lexemes := h ++. lexemes
            history := rst
            if debug then
              trace("Resetting " ++ str ++ "\n" ++ history.map(fn(x) "\tWhen parsing " ++ x.snd).join("\n") ++ " at " ++ lexemes.head.map(show).default("eof") ++ "\nputting back:\n\t" ++ (h ++. Nil).map(show).join("\n\t"))
        Nil ->
          astError("mismatched reset", null)
    fun delimit(msg)
      history := Cons((ctx _, msg, 0), history)
    fun undelimit(msg)
      match history
        Cons((lexes, str, _), Cons((lexes2, str2, i2), rst)) ->
          if str == msg then
            history := Cons((lexes2 ++ lexes, str2, i2), rst)
          else
            trace("Undelimiting " ++ str ++ " but expected " ++ msg)
            throw("Undelimit error")
        _ ->
          throw("Undelimiting from top")
    fun ppeek()
      match lexemes
        Cons(l, _) -> Just(l)
        Nil -> Nothing
    fun peof()
      if debug then
        trace("Parsing eof\n" ++ history.map(fn(x) "\tWhen parsing " ++ x.snd).join("\n") ++ "\n\nHad leftovers: " ++ lexemes.map(fn(x) x.show).join("\n"))
      match lexemes
        Cons(l, _) ->
          astError("peof expected end of file", l.range)
        Nil ->
          ()
    fun tookInput(str)
      match history
        Cons((_, msg, i), _) ->
          if str != msg then
            trace("Checking if input was taken for " ++ str ++ " but expected " ++ msg)
            throw("Take input error")
          else
            i != 0
        Nil ->
          False
    fun ptrace(s: string)
      if debug then
        trace(s ++ "\n  " ++ history.map(fn(x) "When parsing " ++ x.snd).join("\n\t") ++ " next token: " ++ lexemes.take(1).map(show).join(","))
  mask<local>{p()}


fun pmany(kind: string, l: () -> <ast<l>|e> a): <ast<l>|e> list<a>
  match poption("many " ++ kind, l)
    Just(r) -> Cons(r, pmany(kind, l))
    Nothing -> Nil

fun pmany1(kind: string, l: () -> <ast<l>|e> a, ?range: (l) -> range): <ast<l>|e> list<a>
  match poption("many " ++ kind, l)
    Just(r) -> Cons(r, pmany(kind, l))
    Nothing -> astError("at least 1 of " ++ kind, peek().range)

fun pmanyend(kind: string, l: () -> <ast<l>|e> a, p: () -> <ast<l>|e> b): <ast<l>|e> list<a>
  match poption("many " ++ kind, l)
    Just(r) -> 
      match poption("sep " ++ kind, p)
        Just(_) -> Cons(r, pmanyend(kind, l, p))
        Nothing -> Cons(r, Nil)
    Nothing -> Nil

fun pmanyend1(kind: string, l: () -> <ast<l>|e> a, p: () -> <ast<l>|e> b): <ast<l>|e> list<a>
  match poption("many " ++ kind, l)
    Just(r) -> 
      match poption("sep " ++ kind, p)
        Just(_) -> Cons(r, pmanyend(kind, l, p))

// Parse a token
fun ptoken(msg: string, f: (l) -> <ast<l>|e> maybe<a>, ?show: (l) -> div string, ?range: (l) -> range): <ast<l>|e> a
  val t = peek()
  match f(t) 
    Just(a) -> 
      next() 
      ptrace("Consuming " ++ t.show)
      a
    Nothing ->
      astError("expecting " ++ msg, peek().range)

// Version of maybe that does backtrack
fun pmaybe(str: string, p: () -> <ast<l>|e> a): <ast<l>|e> maybe<a>
  delimit(str)
  with override
    return(r)
      undelimit(str)
      Just(r)
    final ctl astError(e, r) 
      reset(str)
      Nothing
  p()

fun pmaybe-list(p: () -> <ast<l>|e> list<a>): <ast<l>|e> list<a>
  match pmaybe("list", p)
    Just(r) -> r
    Nothing -> []

// Similar to maybe, but throws an error
fun try(str: string, p: () -> <ast<l>|e> a): <ast<l>|e> a
  delimit(str)
  with override
    return(r) 
      undelimit(str)
      r
    final ctl astError(e, r) 
      reset(str)
      astError(e,r)
  p()

// Version of maybe that doesn't backtrack
fun poption(str: string, p: () -> <ast<l>|e> a): <ast<l>|e> maybe<a>
  with pdelimit(str)
  with override
    return(r)
      Just(r)
    final ctl astError(e, r)
      if tookInput(str) then
        astError(e, r)
      else
        Nothing 
  p()

fun pchoicesnb(str: string, ps: list<() -> <ast<l>|e> a>, ?a/range: (l) -> range): <ast<l>|e> a
  with pdelimit(str)
  fun find(ps': list<() -> <ast<l>|e> a>, ?b/range: (l) -> range): <ast<l>|e> a
    match ps'
      Cons(p, rst) -> 
        match poption(str, p)
          Just(r) -> r
          Nothing -> find(rst)
      Nil -> astError("expected " ++ str, peek().b/range)
  find(ps)

fun pchoices(str: string, ps: list<() -> <ast<l>|e> a>, ?a/range: (l) -> range): <ast<l>|e> a
  fun find(ps': list<() -> <ast<l>|e> a>, ?b/range: (l) -> range): <ast<l>|e> a
    match ps'
      Cons(p, rst) -> 
        match pmaybe(str, p)
          Just(r) -> r
          Nothing -> find(rst)
      Nil -> astError("expected " ++ str, peek().b/range)
  find(ps)

fun parse-error(r: range, e: string)
  astError("invalid syntax" ++ e.list.drop-while(fn(x) x != ':').string, r)

fun p-braced(p, pL, pR)
  pL()
  val x = p()
  pR()
  x

val p-sep-by = pmanyend
val p-sep-by1 = pmanyend1

fun prange(msg: string, f: (l) -> <ast<l>|e> bool, ?show: (l) -> div string, ?range: (l) -> range): <ast<l>|e> range
  ptoken(msg, fn(x) if f(x) then Just(x) else Nothing).range

fun unimplemented()
  astError("unimplemented", peek().range)