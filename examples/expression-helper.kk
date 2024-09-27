import parsing/fixpoint
import std/data/rb-map
import std/data/rb-set
import parsers/fixpoint-common
import std/pretty/pprint
import parsing/fix-pretty
import std/core-extras

extend type ruleid
  RExpr(prec: int)
  RRExpr(prec: int, pv: pvalue)

extend type pvalue
  PApp(f: string, args: list<pvalue>)

fun ruleid/order2(r1: ruleid, r2: ruleid): pure order2<ruleid>
  match (r1, r2)
    (RChar(c1), RChar(c2)) -> 
      match c1.order2(c2)
        Lt2(c1', c2') -> Lt2(RChar(c1'), RChar(c2'))
        Eq2(c0') -> Eq2(RChar(c0'))
        Gt2(c2', c1') -> Gt2(RChar(c2'), RChar(c1'))
    (RChar(c1), _) -> Lt2(RChar(c1), r2)
    (_, RChar(c2)) -> Gt2(RChar(c2), r1)
    (RInt, RInt) -> Eq2(RInt)
    (RInt, _) -> Lt2(RInt, r2)
    (_, RInt) -> Gt2(RInt, r1)
    (RToken(s1), RToken(s2)) ->
      match s1.str/order2(s2)
        Lt2(s1', s2') -> Lt2(RToken(s1'), RToken(s2'))
        Eq2(s0') -> Eq2(RToken(s0'))
        Gt2(s2', s1') -> Gt2(RToken(s2'), RToken(s1'))
    (RToken(s1), _) -> Lt2(RToken(s1), r2)
    (_, RToken(s2)) -> Gt2(RToken(s2), r1)
    (RExpr(p1), RExpr(p2)) -> 
      match p1.order2(p2)
        Lt2(p1', p2') -> Lt2(RExpr(p1'), RExpr(p2'))
        Eq2(p0') -> Eq2(RExpr(p0'))
        Gt2(p2', p1') -> Gt2(RExpr(p2'), RExpr(p1'))
    (RExpr(p1), _) -> Lt2(RExpr(p1), r2)
    (_, RExpr(p2)) -> Gt2(RExpr(p2), r1)
    (RRExpr(p1, pv1), RRExpr(p2, pv2)) ->
      match p1.order2(p2)
        Lt2(p1', p2') -> Lt2(RRExpr(p1', pv1), RRExpr(p2', pv2))
        Gt2(p2', p1') -> Gt2(RRExpr(p2', pv2), RRExpr(p1', pv1))
        Eq2(p0') -> 
          match pv1.order2(pv2)
            Lt2(pv1', pv2') -> Lt2(RRExpr(p0', pv1'), RRExpr(p0', pv2'))
            Eq2(pv0') -> Eq2(RRExpr(p0', pv0'))
            Gt2(pv1', pv2') -> Gt2(RRExpr(p0', pv1'), RRExpr(p0', pv2'))
    (RRExpr(p1, pv1), _) ->
      Lt2(RRExpr(p1, pv1), r2)
    (_, RRExpr(p2, pv2)) ->
      Gt2(RRExpr(p2, pv2), r1)
    (REOF, REOF) -> Eq2(REOF)
    (REOF, _) -> Lt2(REOF, r2)
    (_, REOF) -> Gt2(REOF, r1)
    (RTop, RTop) -> Eq2(RTop)
    (RTop, _) -> Lt2(RTop, r2)
    (_, RTop) -> Gt2(RTop, r1)
    _ -> throw("Invalid order2 rid")

fun ruleid/show(a: ruleid): pure string
  match a
    RExpr(p) -> "Expr " ++ p.show
    RRExpr(p, pv) -> "RExpr " ++ p.show ++ " " ++ pv.show
    RInt -> "Int"
    RTop -> "Top"
    REOF -> "EOF"
    RToken(s) -> "Token(" ++ s ++ ")"
    RChar(c) -> "Char(" ++ c.show ++ ")"
    _ -> "other"

fun pvalue/show(a: pvalue): pure string
  match a
    PString(s) -> "String(" ++ s ++ ")"
    PList(l) -> "List(" ++ l.map(show).join(", ") ++ ")"
    PInt(i) -> "Int(" ++ i.show ++ ")"
    PEOF -> "EOF"
    PApp(f, args) -> f ++ "(" ++ args.map(show).join(", ") ++ ")"
    PChar(c) -> "Char(" ++ c.show ++ ")"
    _ -> "other"

fun pvalue/order2(p1: pvalue, p2: pvalue): pure order2<pvalue>
  match (p1, p2)
    (PChar(c1), PChar(c2)) -> 
      match c1.order2(c2)
        Lt2(c1', c2') -> Lt2(PChar(c1'), PChar(c2'))
        Eq2(c0') -> Eq2(PChar(c0'))
        Gt2(c2', c1') -> Gt2(PChar(c2'), PChar(c1'))
    (PChar(c1), _) -> Lt2(PChar(c1), p2)
    (_, PChar(c2)) -> Gt2(PChar(c2), p1)
    (PString(s1), PString(s2)) -> 
      match s1.str/order2(s2)
        Lt2(s1', s2') -> Lt2(PString(s1'), PString(s2'))
        Eq2(s0') -> Eq2(PString(s0'))
        Gt2(s2', s1') -> Gt2(PString(s2'), PString(s1'))
    (PString(s1), _) -> Lt2(PString(s1), p2)
    (_, PString(s2)) -> Gt2(PString(s2), p1)
    (PList(l1), PList(l2)) -> 
      match l1.order2(l2)
        Lt2(l1', l2') -> Lt2(PList(l1'), PList(l2'))
        Eq2(l0') -> Eq2(PList(l0'))
        Gt2(l2', l1') -> Gt2(PList(l2'), PList(l1'))
    (PList(l1), _) -> Lt2(PList(l1), p2)
    (_, PList(l2)) -> Gt2(PList(l2), p1)
    (PMaybe(Just(v1)), PMaybe(Just(v2))) -> 
      match v1.order2(v2)
        Lt2(v1', v2') -> Lt2(PMaybe(Just(v1')), PMaybe(Just(v2')))
        Eq2(v0') -> Eq2(PMaybe(Just(v0')))
        Gt2(v2', v1') -> Gt2(PMaybe(Just(v2')), PMaybe(Just(v1')))
    (PMaybe(Just(v1)), _) -> Lt2(PMaybe(Just(v1)), p2)
    (_, PMaybe(Just(v2))) -> Gt2(PMaybe(Just(v2)), p1)
    (PMaybe(Nothing), PMaybe(Nothing)) -> Eq2(PMaybe(Nothing))
    (PMaybe(Nothing), _) -> Lt2(PMaybe(Nothing), p2)
    (_, PMaybe(Nothing)) -> Gt2(PMaybe(Nothing), p1)
    (PUnit, PUnit) -> Eq2(PUnit)
    (PUnit, _) -> Lt2(PUnit, p2)
    (_, PUnit) -> Gt2(PUnit, p1)
    (PInt(i1), PInt(i2)) -> 
      match i1.order2(i2)
        Lt2(i1', i2') -> Lt2(PInt(i1'), PInt(i2'))
        Eq2(i0') -> Eq2(PInt(i0'))
        Gt2(i2', i1') -> Gt2(PInt(i2'), PInt(i1'))
    (PInt(i1), _) -> Lt2(PInt(i1), p2)
    (_, PInt(i2)) -> Gt2(PInt(i2), p1)
    (PEOF, PEOF) -> Eq2(PEOF)
    (PEOF, _) -> Lt2(PEOF, p2)
    (_, PEOF) -> Gt2(PEOF, p1)
    (PApp(f1, args1), PApp(f2, args2)) -> 
      match f1.str/order2(f2)
        Lt2(f1', f2') -> Lt2(PApp(f1', args1), PApp(f2', args2))
        Gt2(f2', f1') -> Gt2(PApp(f2', args2), PApp(f1', args1))
        Eq2(f0') -> 
          match args1.order2(args2)
            Lt2(args1', args2') -> Lt2(PApp(f0', args1'), PApp(f0', args2'))
            Eq2(args0') -> Eq2(PApp(f0', args0'))
            Gt2(args1', args2') -> Gt2(PApp(f0', args1'), PApp(f0', args2'))
    (PApp(f1, args1), _) -> Lt2(PApp(f1, args1), p2)
    (_, PApp(f2, args2)) -> Gt2(PApp(f2, args2), p1)
    _ -> throw("Invalid order2 pvalue")

fun prule/pretty(r: ruleid): pure doc
  r.show.text

fun pvalue/pretty(v: pvalue): pure doc
  v.show.text