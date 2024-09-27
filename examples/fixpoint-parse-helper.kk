import parsing/fixpoint
import std/data/rb-map
import std/data/rb-set
import parsers/fixpoint-common
import std/pretty/pprint
import parsing/fix-pretty
import std/core-extras

extend type ruleid
  RFun

// Order2
fun pvalue/order2(p1: pvalue, p2: pvalue): pure order2<pvalue>
  match (p1, p2)
    (PChar(c1), PChar(c2)) -> 
      match c1.order2(c2)
        Eq2(c) -> Eq2(PChar(c))
        Lt2(c1', c2') -> Lt2(PChar(c1'), PChar(c2'))
        Gt2(c2', c1') -> Gt2(PChar(c2'), PChar(c1'))
    (PChar, _) -> Lt2(p1, p2)
    (_, PChar) -> Gt2(p2, p1)
    (PList(l1), PList(l2)) -> 
      match l1.order2(l2)
        Eq2(l) -> Eq2(PList(l))
        Lt2(l1', l2') -> Lt2(PList(l1'), PList(l2'))
        Gt2(l2', l1') -> Gt2(PList(l2'), PList(l1'))
    (PList, _) -> Lt2(p1, p2)
    (_, PList) -> Gt2(p2, p1)
    (PString(s1), PString(s2)) ->
      match s1.str/order2(s2)
        Eq2(s) -> Eq2(PString(s))
        Lt2(s1', s2') -> Lt2(PString(s1'), PString(s2'))
        Gt2(s2', s1') -> Gt2(PString(s2'), PString(s1'))
    (PString, _) -> Lt2(p1, p2)
    (_, PString) -> Gt2(p2, p1)
    (PEOF, PEOF) -> Eq2(PEOF)
    _ -> Gt2(p1, p2)

// fun pvalue/(==)(c1: pvalue, c2: pvalue): pure bool
//   match (c1, c2)
//     (PChar(c1), PChar(c2)) -> c1 == c2
//     (PList(l1), PList(l2)) -> eql/(==)(l1, l2)
//     (PEOF, PEOF) -> True
//     (PString(s1), PString(s2)) -> s1 == s2
//     (PList(l1), PList(l2)) -> eql/(==)(l1, l2)
//     _ -> False

fun pvalue/show(c: pvalue): pure string
  match c
    PChar(c) -> c.show
    PList(l) -> l.show
    PString(s) -> s
    PEOF -> "EOF"

fun pvalue/pretty(c: pvalue): pure doc
  c.show.text


// fun ruleid/(==)(a: ruleid, b: ruleid): pure bool
//   match (a, b)
//     (RTop, RTop) -> True
//     (RFun, RFun) -> True
//     (REOF, REOF) -> True
//     (RMany(a1,b1), RMany(a2,b2)) -> a1 == a2 && b1 == b2
//     (RAnyChar, RAnyChar) -> True
//     (RMaybe(a), RMaybe(b)) -> a == b
//     (RToken(s1), RToken(s2)) -> s1 == s2
//     _ -> False

fun ruleid/show(a: ruleid): pure string
  match a
    RTop -> "Top"
    RFun -> "Fun"
    REOF -> "EOF"
    RAnyChar -> "AnyChar"
    RMany(a,b) -> "Many(" ++ a.show ++ ", " ++ b.show ++ ")"
    RMaybe(a) -> "Maybe(" ++ a.show ++ ")"
    RToken(s) -> "Token(" ++ s ++ ")"
    _ -> "other"


fun rid/order2(a: ruleid, b: ruleid): pure order2<ruleid>
  val ax = a
  val bx = b
  match (ax, bx)
    (RTop, RTop) -> Eq2(RTop)
    (RTop, _) -> Gt2(b, RTop)
    (_, RTop) -> Lt2(a, RTop)
    (RFun, RFun) -> Eq2(RFun)
    (RFun, _) -> Gt2(b, RFun)
    (_, RFun) -> Lt2(a, RFun)
    (REOF, REOF) -> Eq2(REOF)
    (REOF, _) -> Gt2(b, REOF)
    (_, REOF) -> Lt2(a, REOF)
    (RMany(a1,b1), RMany(a2,b2)) -> 
      match (a1.order2(a2), b1.order2(b2))
        (Eq2(a'), Eq2(b')) -> Eq2(RMany(a',b'))
        (Lt2(a1', a2'), _) -> Lt2(RMany(a1', b1), RMany(a2', b2))
        (_, Lt2(b1', b2')) -> Lt2(RMany(a1, b1'), RMany(a2, b2'))
        (Gt2(a2', a1'), _) -> Gt2(RMany(a2', b2), RMany(a1', b1))
        (_, Gt2(b2', b1')) -> Gt2(RMany(a2, b2'), RMany(a1, b1'))
    (RMany, _) -> Lt2(a, b)
    (_, RMany) -> Gt2(b, a)
    (RAnyChar, RAnyChar) -> Eq2(RAnyChar)
    (RAnyChar, _) -> Gt2(b, RAnyChar)
    (_, RAnyChar) -> Lt2(a, RAnyChar)
    (RMaybe(a1), RMaybe(a2)) -> 
      match a1.order2(a2)
        Eq2(a') -> Eq2(RMaybe(a'))
        Lt2(a1', a2') -> Lt2(RMaybe(a1'), RMaybe(a2'))
        Gt2(a2', a1') -> Gt2(RMaybe(a2'), RMaybe(a1'))
    (RMaybe, _) -> Lt2(a, b)
    (_, RMaybe) -> Gt2(b, a)
    (RToken(s1), RToken(s2)) -> 
      match s1.str/order2(s2)
        Eq2(s) -> Eq2(RToken(s))
        Lt2(s1', s2') -> Lt2(RToken(s1'), RToken(s2'))
        Gt2(s2', s1') -> Gt2(RToken(s2'), RToken(s1'))
    (RToken, _) -> Lt2(a, b)
    (_, RToken) -> Gt2(b, a)
    _ -> Gt2(a, b)