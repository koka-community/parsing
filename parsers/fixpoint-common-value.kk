import parsing/fixpoint

open type pvalue
  PChar(c: char)
  PString(s: string)
  PList(l: list<pvalue>)
  PMaybe(v: maybe<pvalue>)
  PUnit
  PInt(i: int)
  PEOF

extend type ruleid
  RAnyChar
  RWhite
  RInt
  RAlpha
  RAlphaNum
  RChar(c: char)
  RToken(s: string)
  RChars(k: ruleid)
  RNot(k: ruleid)
  RMaybe(k: ruleid)
  RMany(k: ruleid, requireOne: bool)
  RSepBy(k: ruleid, sep: ruleid, allowTrailing: bool)
  RBracketed(k: ruleid, open: ruleid, close: ruleid)
  RPrecededBy(k: ruleid, pre: ruleid)
  RFollowedBy(k: ruleid, post: ruleid)
  RId(startid: ruleid, restid: ruleid)

fun id/inj(c: pvalue): pvalue
  c

fun id/extract(c: pvalue): exn pvalue
  c

fun unit/inj(a: ()): pvalue
  PUnit

fun unit/extract(c: pvalue): exn ()
  match c
    PUnit -> ()
    _ -> throw("Expected Unit")

fun int/inj(i: int): pvalue
  PInt(i)

fun int/extract(c: pvalue): exn int
  match c
    PInt(i) -> i
    _ -> throw("Expected Int")

fun string/inj(s: string): pvalue
  PString(s)

fun string/extract(c: pvalue): exn string
  match c
    PString(s) -> s
    _ -> throw("Expected String")

fun char/inj(c: char): pvalue
  PChar(c)

fun char/extract(c: pvalue): exn char
  match c
    PChar(c) -> c
    _ -> throw("Expected Char")

fun list/inj(l: list<a>, ?inj: (a) -> pure pvalue): pure pvalue
  PList(l.map(?inj))

fun list/extract(c: pvalue, ?extract: pvalue -> pure a): pure list<a>
  match c
    PList(l) -> l.map(?extract)
    _ -> throw("Expected List")

fun maybe/inj(m: maybe<a>, ?inj: (a) -> pure pvalue): pure pvalue
  match m
    Just(a) -> PMaybe(Just(?inj(a)))
    Nothing -> PMaybe(Nothing)

fun maybe/extract(c: pvalue, ?extract: pvalue -> pure a): pure maybe<a>
  match c
    PMaybe(Just(a)) -> Just(?extract(a))
    PMaybe(Nothing) -> Nothing
    _ -> throw("Expected Maybe")

fun further(s1: sslice, s2: sslice): bool
  s1.count < s2.count // s1 is further
