import parsing/fixpoint
pub import parsers/fixpoint-common-value
import std/core-extras

// Combinators
// A repeat combinator that requires a non-empty list of values parsed by `p`
fun pmany1(rid: ruleid, p: parser<v,t,a,e>, ?inj: (list<a>) -> pure v, ?extract: (v) -> pure list<a>): parse<v,t,e> list<a>
  with memo(RMany(rid, True))
  Cons(p(), choices([{pmany1(rid, p)}, {[]}]))

// A repeat combinator that requires a list of values parsed by `p` (may be empty)
fun pmany(rid: ruleid, p: parser<v,t,a,e>, ?inj: (list<a>) -> pure v, ?extract: (v) -> pure list<a>): parse<v,t,e> list<a>
  with memo(RMany(rid, False))
  choices([{val first = p(); val res = pmany(rid, p); Cons(first, res)}, {[]}])

// A combinator that parses a bracketed expression with `p` as the content parser, and `popen` and `pclose` as the bracketing parsers
fun pbracketed<a,b,c,e>(openid: ruleid,  popen: parser<v,t,b,e>, closeid: ruleid, pclose: parser<v,t,c,e>, pid: ruleid, p: parser<v,t,a,e>
      ,?a/inj: (a) -> pure v, ?a/extract: (v) -> pure a
      ,?b/inj: (b) -> pure v, ?b/extract: (v) -> pure b
      ,?c/inj: (c) -> pure v, ?c/extract: (v) -> pure c): parse<v,t,e> a
  with memo(RBracketed(pid,openid,closeid))
  popen()
  val e = p()
  pclose()
  e

// A combinator that parses a sequence of `p` separated by `psep` with optional trailing separator if `allowTrailing` is true
fun psepby(sepid: ruleid, psep: parser<v,t,b,e>, rid: ruleid, p: parser<v,t,a,e>, allowTrailing: bool=True, 
          ?a/inj: (a) -> pure v, ?a/extract: (v) -> pure a, 
          ?la/inj: (list<a>) -> pure v, ?la/extract: (v) -> pure list<a>, 
          ?mla/inj: (maybe<list<a>>) -> pure v, ?mla/extract: (v) -> pure maybe<list<a>>, 
          ?b/inj: (b) -> pure v, ?b/extract: (v) -> pure b,
          ?bm/inj: (maybe<b>) -> pure v, ?bm/extract: (v) -> pure maybe<b>): parse<v,t,e> list<a>
  with memo(RSepBy(rid, sepid, allowTrailing))
  val res = pmaybe(rid)
    val first = p()
    val rest = pmany(RPrecededBy(rid, sepid), {psep(); p()})
    match rest
      [] -> Cons(first, Nil)
      rest' -> 
        if (allowTrailing) then 
          pmaybe(sepid, psep)
          ()
        Cons(first, rest')
  res.default([])

// A combinator that succeeds with `()` when `p` fails
fun pnot(rid: ruleid, p: parser<v,t,a,e>, ?a/inj: (a) -> pure v, ?a/extract: (v) -> pure a, 
                                          ?u/inj: (()) -> pure v, ?u/extract: (v) -> pure ()): parse<v,t,e> ()
  with memo(RNot(rid))
  choices([{p(); none()}, {()}])

// A combinator that succeeds with `Just` when `p` succeeds, and `Nothing` when `p` fails
fun pmaybe(rid: ruleid, p: parser<v,t,a,e>, ?inj: maybe<a> -> pure v, ?extract: v -> pure maybe<a>): parse<v,t,e> maybe<a>
  with memo(RMaybe(rid))
  choices([{Just(p())}, {Nothing}])

// Other common primitives (specialized to parsing string slices)
// A combinator that parses a sequence of characters that satisfy the predicate `pred`
fun pmanychar(nm: ruleid, pred: (char) -> bool): parse<pvalue,sslice,<>> string
  with memo(RChars(nm))
  val orig = get-slice()
  var start := False
  fun p(m: sslice, acc: sslice)
    match m.next()
      Nothing -> 
        if start then 
          set-slice(m)
          acc.string
        else none()
      Just((c1, m'))-> 
        if pred(c1) then 
          start := True
          p(m', acc.extend(1))
        else
          // trace("No more chars " ++ nm ++ start.show)
          if start then
            set-slice(m)
            acc.string
          else
            none()
  p(orig, orig.truncate)

// A combinator that parses a string matching `s` (though typically for keywords you want a whitespace or other token separator after this)
fun ptoken(s: string): parse<pvalue,sslice,<>> string
  with memo(RToken(s))
  val orig = get-slice()
  fun p(m: sslice, r: sslice)
    match (m.next(), r.next())
      (Nothing, Nothing) -> 
        set-slice(m)
        s
      (_, Nothing) -> 
        set-slice(m)
        s
      (Just((c1, m')), Just((c2, r'))) -> 
        if c1 == c2 then p(m', r')
        else none()
      _ -> none()
  p(orig, s.slice)

// A combinator that matches a character `c`
fun match/pchar(c: char): parse<pvalue,sslice,<>> char
  with memo(RChar(c))
  match get-slice().next()
    Just((c1, s')) -> 
      if c1 == c then 
        set-slice(s')
        c
      else none()
    Nothing -> none()

// A combinator that matches a character that satisfies the predicate `pred` and returns that character.
// Note:
//   For many characters prefer `pmanychar` instead of combining this with `pmany` which would create memoization and backtracking points for each character
fun pred/pchar(rid: ruleid, pred: (char) -> bool): parse<pvalue,sslice,<>> char
  with memo(rid)
  match get-slice().next()
    Just((c1, s')) -> 
      if pred(c1) then 
        set-slice(s')
        c1
      else none()
    Nothing -> none()

// Parses an identifier which starts with a character satisfying the `is-start` predicate and is followed by characters satisfying the `is-rest` predicate 
fun pid(sid: ruleid=RAlpha, rid: ruleid=RAlphaNum, is-start: (char) -> bool=is-alpha, is-rest: (char) -> bool=is-alpha-num): parse<pvalue,sslice,<>> string
  with memo(RId(sid, rid))
  var start := False
  fun p(m: sslice, acc: sslice)
    match m.next()
      Nothing -> 
        if start then 
          set-slice(m)
          acc.string
        else none()
      Just((c1, m')) -> 
        if !start then 
          if c1.is-start then 
            start := True
            p(m', acc.extend(1))
          else none()
        else 
          if c1.is-rest then p(m', acc.extend(1))
          else
            set-slice(m)
            acc.string
  val s = get-slice()
  p(s, s.truncate)

// Parses an integer with an optional sign
// 
// TODO: Follow Shrubbery convention for numbers (currently always requires whitespace prior to a sign)
// The + and - characters as a number prefix versus an operator are also subject to a special rule: 
// they are parsed as operators when immediately preceded by an alphanumeric character, _, ., ), ], or } with no whitespace in between. 
// For example, 1+2 is 1 plus 2, but 1 +2 is 1 followed by the number +2.
fun pint(): parse<pvalue,sslice,<>> int
  with memo(RInt)
  val neg = each([{ptoken(" -")}, {ptoken(" +")}, {"+"}]).trim() == "-"
  var start := False
  fun p(m: sslice, acc: sslice)
    match m.next()
      Nothing -> 
        if start then
          match parse-int(acc.string)
            Nothing -> none()
            Just(i) -> 
              set-slice(m)
              if neg then 0 - i else i
        else none()
      Just((c1, m')) -> 
        if c1.is-digit then 
          start := True
          p(m', acc.extend(1))
        else
          if !start then none()
          else
            match parse-int(acc.string)
              Nothing -> none()
              Just(i) -> 
                set-slice(m)
                if neg then 0 - i else i
  val s = get-slice()
  p(s, s.truncate)

// Parses any character
fun any/pchar(): parse<pvalue,sslice,<>> char
  with memo(RAnyChar)
  match get-slice().next()
    Nothing -> none()
    Just((c, s')) -> 
      set-slice(s')
      c

// A parser that succeeds if the end of input is reached 
fun peof(): parse<pvalue,sslice,<>> pvalue
  with memo(REOF)
  match get-slice().next()
    Nothing -> PEOF
    Just(_) -> none()

// Deriviative parsers based on primitives

// Parses many spaces (tabs and spaces) not including newlines
fun pspaces(): parse<pvalue,sslice,<>> string
  pmanychar(RWhite, fn(i) i.is-space)

// Parses an optional sequence of spaces
fun pmspaces()
  pmaybe(RChars(RWhite), fn() pspaces())

// Parses a token followed by optional spaces
fun ptokenw(s: string)
  val t = ptoken(s)
  pmspaces()
  t

// Parses an identifier followed by optional spaces
fun pidw(id: ruleid=RAlpha, rid: ruleid=RAlphaNum, is-start: (char) -> bool=is-alpha, is-rest: (char) -> bool=is-alpha-num)
  val res = pid(id, rid,is-start,is-rest)
  pmspaces()
  res

fun pintw()
  val i = pint()
  pmspaces()

fun popenparen()
  pchar('(')

fun pcloseparen()
  pchar(')')

fun popenbracket()
  pchar('[')

fun pclosebracket()
  pchar(']')

fun popencurly()
  pchar('{')

fun pclosecurly()
  pchar('}')

fun popenangle()
  pchar('<')

fun pcloseangle()
  pchar('>')

fun pdquote()
  pchar('"')

fun psquote()
  pchar('\'')

fun pcomma()
  pchar(',')

fun pcolon()
  pchar(':')

fun psemi()
  pchar(';')

fun pdot()
  pchar('.')

fun ptilde()
  pchar('~')

fun pstar()
  pchar('*')

fun pplus()
  pchar('+')

fun pbacktick()
  pchar('`')

fun pbar()
  pchar('|')

fun pbackslash()
  pchar('\\')

fun pslash()
  pchar('/')

fun pcaret()
  pchar('^')

fun pand()
  pchar('&')

fun ppercent()
  pchar('%')

fun pat()
  pchar('@')

fun pbang()
  pchar('!')

fun phash()
  pchar('#')

fun pdollar()
  pchar('$')

fun pequal()
  pchar('=')

fun pquestion()
  pchar('?')

fun pdash()
  pchar('-')

fun punderscore()
  pchar('_')

fun parrow()
  ptoken("->")

fun palpha()
  pchar(RAlpha, is-alpha)

fun palphanum()
  pchar(RAlphaNum, is-alpha-num)
