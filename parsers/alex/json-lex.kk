/// LINE 1 "../parsing/parsers/alex/json-alex.x"
import std/data/json
import std/core/bslice
import std/core-extras
import std/num/float64

type jslex
  JSStr(str: sslice)
  JSNum(num: sslice)
  JSComment(str: sslice)
  JSTrue
  JSFalse
  JSNull
  JSObjOpen
  JSObjColon
  JSObjClose
  JSArrayOpen
  JSArrayClose
  JSValueSep
  JSWhite

alias action = sslice -> pure jslex
alias alex-input = sslice
alias alex-user = ()
fun alexGetByte(s: alex-input): maybe<(char, alex-input)>
  s.next()

fun alexInputPrevChar(s: alex-input): char
  '_'

extern import
  c file "inline/json-lex"

val alex_tab_size:int = 8
val alex-base = alex-base_create()
extern alex-base_create(): vector<int>
  c inline "kk_intx_t arr[] = {1,0,-93,0,0,0,0,0,0,0,0,-27,-80,-96,-97,-70,-82,-74,-72,-79,21,87,125,148,172,290,325,225,-10,0,311,114,203,321,335,-6,345,0,-62};\nkk_alex_vector_from_cintarray(arr, 39, kk_context())"
  js inline "[1,0,-93,0,0,0,0,0,0,0,0,-27,-80,-96,-97,-70,-82,-74,-72,-79,21,87,125,148,172,290,325,225,-10,0,311,114,203,321,335,-6,345,0,-62]"
val alex-table = alex-table_create()
extern alex-table_create(): vector<int>
  c inline "kk_intx_t arr[] = {0,29,29,29,29,29,29,29,29,29,29,29,29,29,29,3,33,17,33,15,38,32,32,32,32,32,32,32,32,32,32,1,14,29,13,25,2,26,18,37,31,29,29,0,36,4,36,0,28,35,34,34,34,34,34,34,34,34,34,7,0,0,0,11,0,0,0,0,0,23,23,23,23,23,23,23,23,23,23,0,0,0,0,0,0,0,23,23,23,23,23,23,8,0,9,11,29,0,0,0,0,0,0,12,0,0,0,0,0,0,0,19,0,0,0,0,0,16,23,23,23,23,23,23,5,0,6,0,0,29,0,0,0,0,29,25,25,25,25,25,25,25,25,25,25,0,0,0,0,0,0,0,25,25,25,25,25,25,0,0,0,29,30,30,30,30,30,30,30,30,30,30,0,21,21,21,21,21,21,21,21,21,21,0,25,25,25,25,25,25,21,21,21,21,21,21,22,22,22,22,22,22,22,22,22,22,25,0,0,0,0,0,0,22,22,22,22,22,22,25,0,0,21,21,21,21,21,21,0,0,0,0,0,0,0,27,27,27,27,0,0,0,0,0,0,22,22,22,22,22,22,32,32,32,32,32,32,32,32,32,32,0,0,0,25,27,27,0,0,0,25,0,0,0,25,0,0,0,0,0,0,0,25,0,0,0,25,0,25,20,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,27,27,27,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,27,30,30,30,30,30,30,30,30,30,30,32,32,32,32,32,32,32,32,32,32,0,11,31,24,34,34,34,34,34,34,34,34,34,34,35,34,34,34,34,34,34,34,34,34,0,11,0,0,0,0,0,0,0,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};\nkk_alex_vector_from_cintarray(arr, 601, kk_context())"
  js inline "[0,29,29,29,29,29,29,29,29,29,29,29,29,29,29,3,33,17,33,15,38,32,32,32,32,32,32,32,32,32,32,1,14,29,13,25,2,26,18,37,31,29,29,0,36,4,36,0,28,35,34,34,34,34,34,34,34,34,34,7,0,0,0,11,0,0,0,0,0,23,23,23,23,23,23,23,23,23,23,0,0,0,0,0,0,0,23,23,23,23,23,23,8,0,9,11,29,0,0,0,0,0,0,12,0,0,0,0,0,0,0,19,0,0,0,0,0,16,23,23,23,23,23,23,5,0,6,0,0,29,0,0,0,0,29,25,25,25,25,25,25,25,25,25,25,0,0,0,0,0,0,0,25,25,25,25,25,25,0,0,0,29,30,30,30,30,30,30,30,30,30,30,0,21,21,21,21,21,21,21,21,21,21,0,25,25,25,25,25,25,21,21,21,21,21,21,22,22,22,22,22,22,22,22,22,22,25,0,0,0,0,0,0,22,22,22,22,22,22,25,0,0,21,21,21,21,21,21,0,0,0,0,0,0,0,27,27,27,27,0,0,0,0,0,0,22,22,22,22,22,22,32,32,32,32,32,32,32,32,32,32,0,0,0,25,27,27,0,0,0,25,0,0,0,25,0,0,0,0,0,0,0,25,0,0,0,25,0,25,20,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,0,10,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,27,27,27,27,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,0,27,30,30,30,30,30,30,30,30,30,30,32,32,32,32,32,32,32,32,32,32,0,11,31,24,34,34,34,34,34,34,34,34,34,34,35,34,34,34,34,34,34,34,34,34,0,11,0,0,0,0,0,0,0,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,11,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]"
val alex-check = alex-check_create()
extern alex-check_create(): vector<int>
  c inline "kk_intx_t arr[] = {-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,108,43,97,45,115,117,48,49,50,51,52,53,54,55,56,57,101,114,32,108,34,108,47,117,101,46,40,41,-1,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,58,-1,-1,-1,69,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,91,-1,93,101,95,-1,-1,-1,-1,-1,-1,102,-1,-1,-1,-1,-1,-1,-1,110,-1,-1,-1,-1,-1,116,97,98,99,100,101,102,123,-1,125,-1,-1,128,-1,-1,-1,-1,133,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,-1,-1,-1,160,48,49,50,51,52,53,54,55,56,57,-1,48,49,50,51,52,53,54,55,56,57,-1,97,98,99,100,101,102,65,66,67,68,69,70,48,49,50,51,52,53,54,55,56,57,34,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,47,-1,-1,97,98,99,100,101,102,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,-1,-1,-1,-1,-1,-1,97,98,99,100,101,102,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,92,40,41,-1,-1,-1,98,-1,-1,-1,102,-1,-1,-1,-1,-1,-1,-1,110,-1,-1,-1,114,-1,116,117,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,-1,-1,34,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,-1,133,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,-1,69,46,92,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,-1,69,-1,-1,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};\nkk_alex_vector_from_cintarray(arr, 601, kk_context())"
  js inline "[-1,0,1,2,3,4,5,6,7,8,9,10,11,12,13,108,43,97,45,115,117,48,49,50,51,52,53,54,55,56,57,101,114,32,108,34,108,47,117,101,46,40,41,-1,43,44,45,-1,47,48,49,50,51,52,53,54,55,56,57,58,-1,-1,-1,69,-1,-1,-1,-1,-1,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,91,-1,93,101,95,-1,-1,-1,-1,-1,-1,102,-1,-1,-1,-1,-1,-1,-1,110,-1,-1,-1,-1,-1,116,97,98,99,100,101,102,123,-1,125,-1,-1,128,-1,-1,-1,-1,133,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,-1,-1,-1,160,48,49,50,51,52,53,54,55,56,57,-1,48,49,50,51,52,53,54,55,56,57,-1,97,98,99,100,101,102,65,66,67,68,69,70,48,49,50,51,52,53,54,55,56,57,34,-1,-1,-1,-1,-1,-1,65,66,67,68,69,70,47,-1,-1,97,98,99,100,101,102,-1,-1,-1,-1,-1,-1,-1,10,11,12,13,-1,-1,-1,-1,-1,-1,97,98,99,100,101,102,48,49,50,51,52,53,54,55,56,57,-1,-1,-1,92,40,41,-1,-1,-1,98,-1,-1,-1,102,-1,-1,-1,-1,-1,-1,-1,110,-1,-1,-1,114,-1,116,117,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,-1,-1,34,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,-1,133,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,-1,69,46,92,48,49,50,51,52,53,54,55,56,57,48,49,50,51,52,53,54,55,56,57,-1,69,-1,-1,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,101,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]"
val alex-deflt = alex-deflt_create()
extern alex-deflt_create(): vector<int>
  c inline "kk_intx_t arr[] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,25,26,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};\nkk_alex_vector_from_cintarray(arr, 39, kk_context())"
  js inline "[-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,25,26,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1]"
val alex-accept : some<e> vector<alex-acc<e>> = ( [ AlexAccNone, AlexAcc(17), AlexAccNone, AlexAcc(16), AlexAcc(15), AlexAcc(14), AlexAcc(13), AlexAcc(12), AlexAcc(11), AlexAcc(10), AlexAcc(9), AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAccNone, AlexAcc(8), AlexAcc(7), AlexAcc(6), AlexAcc(5), AlexAcc(4), AlexAccNone, AlexAcc(3), AlexAccNone, AlexAcc(2), AlexAcc(1), AlexAccNone, AlexAcc(0), AlexAccNone ] : some<e> list<alex-acc<e>>).vector;

val alex-actions : vector<action> = vector-init-list(18, [ (17,alex_action_4), (16,alex_action_5), (15,alex_action_6), (14,alex_action_7), (13,alex_action_8), (12,alex_action_9), (11,alex_action_10), (10,alex_action_11), (9,alex_action_12), (8,alex_action_0), (7,alex_action_0), (6,alex_action_1), (5,alex_action_1), (4,alex_action_2), (3,alex_action_2), (2,alex_action_2), (1,alex_action_2), (0,alex_action_3) ] : list<(int,action)>);

val alex_action_0: action = fn(s:sslice) JSComment(s.advance(2))
val alex_action_1: action = fn(s:sslice) JSWhite
val alex_action_2: action = fn(s:sslice) JSNum(s)
val alex_action_3: action = fn(s:sslice) JSTrue
val alex_action_4: action = fn(s:sslice) JSFalse
val alex_action_5: action = fn(s:sslice) JSNull
val alex_action_6: action = fn(s:sslice) JSValueSep
val alex_action_7: action = fn(s:sslice) JSObjOpen
val alex_action_8: action = fn(s:sslice) JSObjClose
val alex_action_9: action = fn(s:sslice) JSObjColon
val alex_action_10: action = fn(s:sslice) JSArrayOpen
val alex_action_11: action = fn(s:sslice) JSArrayClose
val alex_action_12: action = fn(s:sslice) JSStr(s.advance(1).extend(-2))

// -----------------------------------------------------------------------------
// ALEX TEMPLATE
// -----------------------------------------------------------------------------
// This code is in the PUBLIC DOMAIN; you may copy it freely and use
// it for any purpose whatsoever.

// -----------------------------------------------------------------------------
// Main lexing routines

type alex-return
  AlexEOF
  AlexError(i: alex-input)
  AlexSkip(i: alex-input, n: int)
  AlexToken(i: alex-input, n: int, a: action) // a is the action type


// alexScan :: AlexInput -> StartCode -> AlexReturn a
fun alexScan(input: alex-input, sc: int): <pure|e> alex-return
  alexScanUser(Nothing, input, sc)

fun alexScanUser(user: maybe<alex-user>, input: alex-input, sc: int): <pure|e> alex-return
  match alex_scan_tkn(user, input, 0, input, sc, AlexNone)
    (AlexNone, input__') ->
      match alexGetByte(input)
        Nothing ->
          AlexEOF
        Just ->
          AlexError(input__')
    (AlexLastSkip(input__'', len), _) ->
      AlexSkip(input__'', len)
    (AlexLastAcc(k, input__''', len), _) ->
      AlexToken(input__''', len, alex-actions.unsafe-idx(k.ssize_t))

// Push the input through the DFA, remembering the most recent accepting
// state it encountered.
fun alex_scan_tkn(user__:maybe<alex-user>, orig_input:alex-input, len:int, input__:alex-input, s: int, last_acc: alex-last-acc): <pure|e> (alex-last-acc, alex-input)
  fun check_accs(acc: alex-acc<<|e>>): <|e> alex-last-acc
    match acc
      AlexAccNone -> last_acc
      AlexAcc(a) -> AlexLastAcc(a, input__, len)
      AlexAccSkip -> AlexLastSkip(input__, len)
      AlexAccPred(a, predx, rest)
        -> if predx(user__, orig_input, len, input__) then AlexLastAcc(a, input__, len)
           else check_accs(rest)
      AlexAccSkipPred(predx, rest)
        -> if predx(user__, orig_input, len, input__) then AlexLastSkip(input__, len)
           else check_accs(rest)
  val new_acc = check_accs(alex-accept.unsafe-idx(s.ssize_t))
  match alexGetByte(input__)
     Nothing -> (new_acc, input__)
     Just((c, new_input)) ->
       match c.int
        ord_c ->
          val base = alex-base.unsafe-idx(s.ssize_t)
          val offset = base + ord_c
          val check  = alex-check.unsafe-idx(offset.ssize_t)
          val new_s = if offset >= 0 && check == ord_c
                          then alex-table.unsafe-idx(offset.ssize_t)
                          else alex-deflt.unsafe-idx(s.ssize_t)
        
          match new_s
              -1 -> (new_acc, input__)
                  // on an error, we want to keep the input *before* the
                  // character that failed, not after.
              _ -> alex_scan_tkn(user__, orig_input, len + 1, new_input, new_s, new_acc)
 

type alex-last-acc
  AlexNone
  AlexLastAcc(i: int, a: alex-input, n: int)
  AlexLastSkip(a: alex-input, n: int)

type alex-acc<e>
  AlexAccNone
  AlexAcc(i: int)
  AlexAccSkip
  AlexAccPred(i: int, pred: alex-acc-pred<e>, acc: alex-acc<e>)
  AlexAccSkipPred(pred: alex-acc-pred<e>, acc: alex-acc<e>)

alias alex-acc-pred<e> = (maybe<alex-user>, alex-input, int, alex-input) -> e bool;

// -----------------------------------------------------------------------------
// Predicates on a rule

inline fun alexAndPred(p1: alex-acc-pred<e>, p2: alex-acc-pred<e>, user__: maybe<alex-user>, in1: alex-input, len: int, in2: alex-input): e bool
  p1(user__, in1, len, in2) && p2(user__, in1, len, in2)

// alexPrevCharIsPred :: Char -> AlexAccPred _
inline fun alexPrevCharIs(c: char, _, input__: alex-input, _, _): <pure> bool
  c == alexInputPrevChar(input__)

inline fun alexPrevCharMatches(f: (char) -> pure bool, _, input__: alex-input, _, _): <pure> bool
  f(alexInputPrevChar(input__))

// alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
inline fun alexPrevCharIsOneOf(arr: list<char>, _, input__: alex-input, _, _): <pure> bool
  val prev = alexInputPrevChar(input__)
  arr.any(fn(c) c == prev)

// alexRightContext :: Int -> AlexAccPred _
inline fun alexRightContext(sc:int, user__: maybe<alex-user>, _, _, input__: alex-input): <pure|e> bool
  match alex_scan_tkn(user__, input__, 0, input__, sc, AlexNone)
    (AlexNone, _) -> False
    _ -> True
        // TODO: there's no need to find the longest
        // match when checking the right context, just
        // the first match will do./// LINE 88 "../parsing/parsers/alex/json-alex.x"

