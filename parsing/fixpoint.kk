import std/data/linearmap
import std/data/linearset
import std/fixpoint/lattice

open type ruleid
  REOF // End of file / input
  RTop // Top level rule

// Allows for the cache itself to use nondeterminism (state for the cache should still be above this)
effect take-all
  ctl take-all(xs: list<a>): a

effect fix-debug<v,t>
  // Just some helpers so that we don't need to pass around implicits everywhere for debugging
  fun show-tokens(tokens: t): string
  fun show-value(value: v): string
  fun show-ruleid(ruleid: ruleid): string
  fun eq-ruleid(r1: ruleid, r2: ruleid): bool
  fun eq-tokens(t1: t, t2: t): bool
  fun eq-values(v1: v, v2: v): bool

// Crucially all state for a result must be in the cache
//  - So we can resume at a depend on a memo key with the correct state
//  - Currently this include the tokens and the result

// rule id + slice (t) is the memo key, v is the result
effect cache<v,t>
  // Cache / memoization
  fun is-cached(rid: ruleid, start: t): bool
  ctl depend(rid: ruleid, start: t): (v,t) // Add a dependency, and return the result along with the result's accompanying slice
  fun add-result(rid: ruleid, start: t, result: v, end: t): ()
  // Nondeterminism
  final ctl none(): b
  ctl do-each(fns: list<a>): a
  ctl do-choice(fns: list<a>): a

// current input location
effect slice<t>
  fun get-slice(): t
  fun set-slice(l: t): () // Allows for backtracking / moving forward

// Dependencies, and current values
value struct memo-state<v,t,e>
  deps: list<(((v,t)) -> <pure|e> maybe<(v,t)>)>
  vals: linearSet<(v,t)>;

alias parse<v,t,e> = <pure,take-all,slice<t>,fix-debug<v,t>,cache<v,t>|e>
alias parser<v,t,b,e> = () -> parse<v,t,e> b
alias parse-cache<v,t> = linearMap<ruleid,linearMap<t,linearSet<(v,t)>>>

fun remove-deps(cache: linearMap<ruleid,linearMap<t,memo-state<v,t,e>>>): parse-cache<v,t>
  cache.map(fn(_, m) m.map(fn(_, ms) {
    val l: some<v,t> linearSet<(v,t)> = ms.vals
    l
    }))

fun settuple/join(s: linearSet<(v,t)>, new: (v,t), ?t/(==): (t,t)-> pure bool, ?v/(==): (v,v) -> pure bool): pure (bool, linearSet<(v,t)>)
  if s.member(new, ?(==) = fn(a, b) a.fst == b.fst && a.snd == b.snd) then (False, s)
  else (True, s.add(new, ?(==) = fn(a, b) a.fst == b.fst && a.snd == b.snd))

fun take-best(res: list<maybe<(v,t)>>, ?further: (t,t) -> bool): maybe<(v,t)>
  res.foldl(Nothing) fn(best, r)
    match r 
      Nothing -> best
      Just((r, t)) ->
        match best
          Nothing -> Just((r,t))
          Just((r', t')) ->
            if t.further(t') then Just((r, t))
            else Just((r', t'))

fun handle-debug(action: () -> <fix-debug<v,t>,pure|e> b, 
    ?r/(==): (ruleid, ruleid) -> pure bool, ?r/show: (ruleid) -> pure string,
    ?v/(==): (v, v) -> pure bool, ?v/show: v -> pure string, 
    ?t/(==): (t, t) -> pure bool,  ?t/show: t -> pure string): <pure|e> b
  with handler
    fun eq-ruleid(r1, r2)
      r/(==)(r1, r2)
    fun show-ruleid(rid)
      rid.r/show
    fun show-tokens(t)
      t.t/show
    fun show-value(c)
      c.v/show
    fun eq-tokens(l1, l2)
      t/(==)(l1, l2)
    fun eq-values(c1, c2)
      v/(==)(c1, c2)
  action()

// Runs a parsing fixpoint with `init` tokens and `f` as the parsing function
// 
// Returns the fixpoint cache which is of the form: `map<ruleid, map<tokens, parseresult>>` where `ruleid` is a string
// Tokens can be any type, for e.g. a string based parser, use `sslice` as the token type
// Because we need to be able to cache / memoize the keys and results must be comparable
fun run-fixpoint(init: t, rid: ruleid, parser: parser<v,t,b,e>, ?r/(==): (ruleid, ruleid) -> pure bool, ?r/show: (ruleid) -> pure string,
                                               ?v/(==): (v, v) -> pure bool, ?v/show: v -> pure string, 
                                               ?t/(==): (t, t) -> pure bool,  ?t/show: t -> pure string, 
                                               ?further: (t, t) -> bool, ?inj: b -> pure v): <pure|e> (parse-cache<v,t>, list<(v,t)>)
  var cache : some<v,t,e> linearMap<ruleid,linearMap<t,memo-state<v,t,e>>> := LinearMap([])
  val do = 
    with handle-debug()
    with handler
      ctl take-all(xs)
        xs.foreach(resume)
      return(x) ()
    with handler
      fun add-result(key, start, result, end)
        // trace("Adding result for " ++ s.r/show ++ " " ++ start.t/show ++ " result: " ++ c.v/show ++ " rest " ++ end.t/show)
        match cache.lookup(key)
          Just(tokenMap) ->
            match tokenMap.lookup(start, ?(==) = t/(==))
              Just(Memo-state(deps, vals)) ->
                val (changed, vals') = vals.join((result,end), ?t/(==) = t/(==), ?v/(==) = v/(==))
                if changed then 
                  cache := cache.set(key, tokenMap.set(start, Memo-state(deps, vals')))
                  // trace("  Updating " ++ deps.length.show ++ " deps at " ++ start.t/show ++ " rest " ++ end.t/show)
                  // I think there is a problem here, since we ignore the result of the resume...
                  deps.list/foreach(fn(res) {res((result,end)); ()})
                else ()
          Nothing -> throw("No cache for " ++ key.show)
      fun is-cached(key, start)
        match cache.lookup(key)
          Just(tokenMap) -> 
            if tokenMap.contains-key(start) then 
              True
            else // if not cached, we need to set the key, so for left recursion we don't loop
              cache := cache.set(key, tokenMap.set(start, Memo-state([], LinearSet([]))))
              False
          _ -> // if not cached, we need to set the key, so for left recursion we don't loop
            cache := cache.set(key, LinearMap([(start, Memo-state([], LinearSet([])))]))
            False
      ctl depend(s, start)
        match cache.lookup(s)
          Just(tokenMap) ->
            match tokenMap.lookup(start, ?(==) = t/(==))
              Just(Memo-state(deps, vals)) ->
                cache := cache.set(s, tokenMap.set(start, Memo-state(Cons(fn(r) resume(r), deps), vals)))
                match vals.list.map(fn(cv) resume(cv))
                  [] -> Nothing
                  res -> res.take-all
      // A variant of nondet that backtracks the state (tokens) on failure and takes the longest match
      ctl do-each(fns)
        fns.map-indexed(fn(i, f) resume(f)).take-all
      // Finds the first successful parser
      ctl do-choice(fns) 
        fun fx(fns')
          match fns'
            Nil -> Nothing
            Cons(f, fnss) ->
              match resume(f)
                Just(res) -> Just(res)
                Nothing -> fx(fnss)
        fx(fns)
      final ctl none() Nothing
      return(x) Just(x)
    with token-handler(init)
    parser().inj
  // Remove dependency lists
  val cache' = cache.remove-deps
  (cache', cache'.map/lookup(rid).map(fn(v') v'.lookup(init).map(list).default([])).default([]))

fun parse(init: t, rid: ruleid, parser: parser<v,t,b,e>, 
                        ?r/(==): (ruleid, ruleid) -> pure bool, ?r/show: (ruleid) -> pure string,
                        ?v/(==): (v, v) -> pure bool, ?v/show: v -> pure string, 
                        ?t/(==): (t, t) -> pure bool,  ?t/show: t -> pure string, 
                        ?further: (t, t) -> bool, ?inj: b -> pure v): <pure|e> list<(v,t)>
  run-fixpoint(init, rid, parser).snd

fun parse-cache(init: t, rid: ruleid, parser: parser<v,t,b,e>, 
                        ?r/(==): (ruleid, ruleid) -> pure bool, ?r/show: (ruleid) -> pure string,
                        ?v/(==): (v, v) -> pure bool, ?v/show: v -> pure string, 
                        ?t/(==): (t, t) -> pure bool,  ?t/show: t -> pure string, 
                        ?further: (t, t) -> bool, ?inj: b -> pure v): <pure|e> parse-cache<v,t>
  run-fixpoint(init, rid, parser).fst

fun token-handler(init: t, action: () -> <slice<t>,div|e> v): <div|e> (v, t)
  var tokens := init
  with handler
    fun get-slice()
      tokens
    fun set-slice(t)
      tokens := t
    return(x)
      (x, tokens)
  action()

// Set's up a memo key for the fixpoint, 
// along with the function to run to evaluate if not previously evaluated at this point
fun memo(rid: ruleid, f: () -> parse<v,t,e> a, ?inj: a -> pure v, ?extract: v -> pure a): parse<v,t,e> a
  // trace("Check rule " ++ s.show-ruleid() ++ " at " ++ get-slice().show-tokens())
  val start = get-slice()
  if is-cached(rid, start) then
    val (v,end) = depend(rid, start)
    set-slice(end)
    // trace("  Using cached value for " ++ s.show-ruleid() ++ " " ++ start.show-tokens() ++ " " ++ v.show-value() ++ " rest " ++ end.show-tokens())
    v.extract
  else
    val r = f() // Do we need to depend(s) here as well?
    add-result(rid, start, r.inj, get-slice())
    r

fun fix/each(ls: list<parser<v,t,a,e>>): parse<v,t,e> a
  val start = get-slice()
  val f = do-each(ls)
  set-slice(start)
  f()

fun fix/choices(ls: list<parser<v,t,a,e>>): parse<v,t,e> a
  val start = get-slice()
  val f = do-choice(ls)
  set-slice(start)
  f()