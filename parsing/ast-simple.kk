/*----------------------------------------------------------------------------
   Copyright 2024, Koka-Community Authors

   Licensed under the MIT License ("The License"). You may not
   use this file except in compliance with the License. A copy of the License
   can be found in the LICENSE file at the root of this distribution.
----------------------------------------------------------------------------*/
import std/core/undiv

effect astError
  final ctl astError(e: string): a

effect astParse<l>
  fun addWarning(w: string): ()
  fun ppeek(): maybe<l>
  fun undelimit(msg: string): ()
  fun reset(msg: string): ()
  fun delimit(msg: string): ()
  fun progress-since(msg: string): bool 
  fun pnext(): maybe<l>
  fun peof(): ()
  fun tookInput(msg: string): bool
  fun ptrace(s: string): ()

fun progress(msg: string, f: () -> <ast<l>|e> a): <ast<l>|e> a
  if progress-since(msg) then
    match pmaybe(msg, f)
      Just(x) -> x
      _ -> astError(msg ++ " did not consume input")
  else
    astError(msg ++ " did not consume input")

fun pdelimit(msg: string, f: () -> <ast<l>|e> a): <ast<l>|e> a
  delimit(msg)
  with override
    final ctl astError(e) 
      undelimit(msg)
      astError("\nWhen parsing " ++ msg ++ " " ++ e)
  val x = f()
  undelimit(msg)
  x

fun peek(): ast<l> l
  val p = ppeek()
  match p
    Just(l) -> l
    Nothing -> astError("unexpected end of file")

fun next(): ast<l> l
  val p = pnext()
  match p
    Just(l) -> l
    Nothing -> astError("unexpected end of file")

alias ast<l> = <astParse<l>, astError, pure>
fun lexParse(input: string, p: () -> <console,ast<l>|e> a, ?lex: (string) -> <console, pure, astError|e> list<l>, ?show: (l) -> pure string): <console,astError,pure|e> a
  val lexes = lex(input)
  parseLexemes(lexes, p)
  
fun parseLexemes(lexemes0: list<l>, p: () -> <console,ast<l>|e> a, ?show: (l) -> pure string): <console,astError,pure|e> a
  var lexemes := lexemes0
  var history := Cons((ctx _, "top", 0), Nil)
  val debug = True
  with override
    final ctl astError(e: string)
      astError("Got error: " ++ e ++ "\n" ++ " when parsing:\n\t" ++ lexemes.take(2).map(show).join("\n\t"))
  with handler
    fun addWarning(w: string)
      ()
    fun pnext()
      match lexemes
        Cons(l, rst) ->
          // trace("Consuming " ++ l.show ++ " from " ++ history.take(10).reverse.map(snd).show)
          lexemes := rst 
          match history
            Cons((h, msg, i), tl) ->
              history := Cons((h ++ ctx Cons(l, _), msg, i + 1), tl)
            Nil -> 
              astError("unexpected top of context")
          Just(l)
        Nil -> Nothing
    fun reset(msg)
      match history
        Cons((h, str, _), rst) -> 
          if str != msg then
            // trace("Resetting " ++ str ++ " but expected " ++ msg)
            throw("Reset error")
          else
            lexemes := h ++. lexemes
            history := rst
            // if debug then
            //   trace("Resetting " ++ str ++ "\n" ++ history.map(fn(x) "\tWhen parsing " ++ x.snd).join("\n") ++ " at " ++ lexemes.head.map(show).default("eof") ++ "\nputting back:\n\t" ++ (h ++. Nil).map(show).join("\n\t"))
        Nil ->
          astError("mismatched reset")
    fun delimit(msg)
      // trace("Trying  " ++ history.take(10).reverse.map(snd).show ++ " " ++ msg)
      history := Cons((ctx _, msg, 0), history)
    fun progress-since(msg)
      fun progress(h)
        match h
          Cons((_, str, 0), _) | str == msg -> False
          Cons((_, _, n), _) | n != 0 -> True
          Cons(_, rst) -> progress(rst)
          Nil -> True // Never been this call so sure
      progress(history)
    fun undelimit(msg)
      match history
        Cons((lexes, str, _), Cons((lexes2, str2, i2), rst)) ->
          if str == msg then
            history := Cons((lexes2 ++ lexes, str2, i2), rst)
          else
            // trace("Undelimiting " ++ str ++ " but expected " ++ msg)
            throw("Undelimit error")
        _ ->
          throw("Undelimiting from top")
    fun ppeek()
      match lexemes
        Cons(l, _) -> Just(l)
        Nil -> Nothing
    fun peof()
      // if debug then
        // trace("Parsing eof\n" ++ history.map(fn(x) "\tWhen parsing " ++ x.snd).join("\n") ++ "\n\nHad leftovers: " ++ lexemes.map(fn(x) x.show).join("\n"))
      match lexemes
        Cons(l, _) ->
          astError("peof expected end of file" ++ l.show)
        Nil ->
          ()
    fun tookInput(str)
      match history
        Cons((_, msg, i), _) ->
          if str != msg then
            // trace("Checking if input was taken for " ++ str ++ " but expected " ++ msg)
            throw("Take input error")
          else
            i != 0
        Nil ->
          False
    fun ptrace(s: string)
      if debug then
        trace(s ++ "\n  " ++ history.map(fn(x) "When parsing " ++ x.snd).join("\n\t") ++ " next token: " ++ lexemes.take(1).map(show).join(","))
  mask<local>{p()}


fun pmany(kind: string, l: () -> <ast<t>|e> a): <ast<t>|e> list<a>
  match poption("many " ++ kind, l)
    Just(r) -> Cons(r, pmany(kind, l.pretend-decreasing))
    Nothing -> Nil

fun pmany1(kind: string, l: () -> <ast<l>|e> a): <ast<l>|e> list<a>
  match poption("many " ++ kind, l)
    Just(r) -> Cons(r, pmany(kind, l.pretend-decreasing))
    Nothing -> astError("at least 1 of " ++ kind)

fun pmanyend(kind: string, l: () -> <ast<l>|e> a, p: () -> <ast<l>|e> b): <ast<l>|e> list<a>
  match poption("many " ++ kind, l)
    Just(r) -> 
      match poption("sep " ++ kind, p)
        Just(_) -> Cons(r, pmanyend(kind, l.pretend-decreasing, p.pretend-decreasing))
        Nothing -> Cons(r, Nil)
    Nothing -> Nil

fun pmanyend1(kind: string, l: () -> <ast<l>|e> a, p: () -> <ast<l>|e> b): <ast<l>|e> list<a>
  match poption("many " ++ kind, l)
    Just(r) -> 
      match poption("sep " ++ kind, p)
        Just(_) -> Cons(r, pmanyend(kind, l.pretend-decreasing, p.pretend-decreasing))
        Nothing -> Cons(r, Nil)
    Nothing -> astError("at least 1 of " ++ kind)

// Parse a token
fun ptoken(msg: string, f: (l) -> <ast<l>|e> maybe<a>, ?show: (l) -> pure string): <ast<l>|e> a
  val t = peek()
  match f(t) 
    Just(a) -> 
      next()
      // ptrace("Consuming " ++ t.show)
      a
    Nothing ->
      astError("expecting " ++ msg)

// Version of maybe that does backtrack
fun pmaybe(str: string, p: () -> <ast<l>|e> a): <ast<l>|e> maybe<a>
  delimit(str)
  with override
    return(r)
      undelimit(str)
      Just(r)
    final ctl astError(e) 
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
    final ctl astError(e) 
      reset(str)
      astError(e)
  p()

// Version of maybe that doesn't backtrack
fun poption(str: string, p: () -> <ast<l>|e> a): <ast<l>|e> maybe<a>
  with pdelimit(str)
  with override
    return(r)
      Just(r)
    final ctl astError(e)
      if tookInput(str) then
        astError(e)
      else
        Nothing 
  p()

fun pchoicesnb(str: string, ps: list<() -> <ast<l>|e> a>): <ast<l>|e> a
  with pdelimit(str)
  fun find(ps': list<() -> <ast<l>|e> a>): <ast<l>|e> a
    match ps'
      Cons(p, rst) -> 
        match poption(str, p)
          Just(r) -> r
          Nothing -> find(rst)
      Nil -> astError("expected " ++ str)
  find(ps)

fun pchoices(str: string, ps: list<() -> <ast<l>|e> a>): <ast<l>|e> a
  try(str)
    fun find(ps': list<() -> <ast<l>|e> a>): <ast<l>|e> a
      match ps'
        Cons(p, rst) -> 
          match pmaybe(str, p)
            Just(r) -> r
            Nothing -> find(rst)
        Nil -> astError("expected " ++ str)
    find(ps)

fun parse-error(e: string): astError a
  astError("invalid syntax" ++ e.list.drop-while(fn(x) x != ':').string)

fun p-braced(pL, pR, p)
  pL()
  val x = p()
  pR()
  x

val p-sep-by = pmanyend
val p-sep-by1 = pmanyend1

fun unimplemented(): astError a
  astError("unimplemented")