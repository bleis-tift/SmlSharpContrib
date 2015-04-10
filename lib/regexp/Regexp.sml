structure Regexp = 
struct
open RegexpParser

datatype result
  = Success of int * int * ((int * int) array)
  | Continue
  | Fail
        

fun match_aux(r, rest, str, i, gs) =
  case r of
      Item c => ((if String.sub(str, i) = c
                  then Success(i, i+1, gs)
                  else Continue)
                 handle Subscript => Fail)
    | LineStart => ((if i = 0  orelse String.sub(str, i - 1) = #"\n"
                     then Success(i, i, gs)
                     else Continue)
                    handle Subscript => Fail)
    | LineEnd => ((if i = (String.size str) orelse String.sub(str, i) = #"\n"
                   then Success(i, i, gs)
                   else Continue)
                  handle Subscript => Fail)
    | Or [] => if i = (String.size str)
               then Fail
               else Continue
    | Or (x :: xs) => (case match_aux(x, rest, str, i, gs) of
                           Success(s, e, gs) => (case match_aux(rest, Empty, str, e, gs) of (* backtrack *)
                                                 Success _ => Success(s, e, gs)
                                               | Continue => match_aux(Or xs, rest, str, i, gs)
                                               | Fail => match_aux(Or xs, rest, str, i, gs))
                         | Continue => match_aux(Or xs, rest, str, i, gs)
                         | Fail => match_aux(Or xs, rest, str, i, gs))
    | And [] => Success(i, i, gs)
    | And (x :: xs) => (case (case match_aux(x, And xs, str, i, gs) of
                                  Success(_, e, gs) => match_aux(And xs, rest, str, e, gs)
                                | Continue => Continue
                                | Fail => Fail) of
                            Success(_, e, gs) => Success(i, e, gs)
                          | Continue => Continue
                          | Fail => Fail)
    | Kleene x => (case match_aux(x, rest, str, i, gs) of
                       Success(_, e, gs) =>
                       (case match_aux(rest, Empty, str, e, gs) of (* for backtrack *)
                            Success _ =>   (* can backtrack *)
                            (case match_aux(r, rest, str, e, gs) of (* try procede *)
                                 Success(_, e', gs) => Success(i, e', gs)
                               | _ => Success(i, e, gs))
                          | _ => (* need procede or backtrack *)
                            (case match_aux(r, rest, str, e, gs) of
                                 Success(_, e', gs) => Success(i, e', gs)
                               | _ => (case match_aux(rest, Empty, str, i, gs) of
                                           Success _ => Success(i, i, gs)
                                         | Continue => Continue
                                         | Fail => Fail)))
                     | _ => (case match_aux(rest, Empty, str, i, gs) of
                                 Success _ => Success(i, i, gs)
                               | Continue => Continue
                               | Fail => Fail))
    | Group(gi, x) => (case match_aux(x, rest, str, i, gs) of
                           Success(s, e, gs) => (Array.update(gs, gi, (s, e));
                                                 Success(s, e,  gs))
                         | Continue=> Continue
                         | Fail => Fail)
    | Any => ((if 0 <= i andalso (i < String.size str)
               then Success(i, i+1, gs)
               else Continue)
              handle Subscript => Fail)
    | Empty => Success(i, i, gs)

fun match(r as (ast, gi), str, i) =
  case match_aux(ast, Empty, str, i, Array.array(gi, (0, 0))) of
      Success(s, e, gs) => SOME(s, e, gs)
    | Continue => match(r, str, i + 1)
    | Fail => NONE

fun doesMatch(r, str, i) =
  case match(r, str, i) of
      SOME _ => true
    | NONE => false

fun matchString(r, str, i) =
  case match(r, str, i) of
      SOME(s, e, gs) => let
       val gs' = Array.array(Array.length gs, "")
       val _ = Array.appi (fn(i, (s, e))=> Array.update(gs', i, String.substring(str, s, e - s))) gs
   in
       SOME(String.substring(str, s, e - s), gs')
   end
    | NONE => NONE

fun matchStrings(r, str, i) =
  case match(r, str, i) of
      SOME(s, e, _) => String.extract(str, s, SOME (e - s)) :: matchStrings(r, str, e)
    | NONE => []

fun split(a, str, i) =
  case match(a, str, i) of
      SOME(s, e, _) => String.extract(str, i, SOME (s - i)) :: split(a, str, e)
    | NONE => [String.extract(str, i, NONE)]

fun replace(a, str, i, new) =
  case match(a, str, i) of
      SOME(s, e, _) => String.extract(str, i, SOME (s - i)) ^ new ^ (String.extract(str,e , NONE))
    | NONE =>  str

fun replaceAll(a, str, i, new) =
  case match(a, str, i) of
      SOME(s, e, _) =>  String.extract(str, i, SOME (s - i)) ^ new ^ (replaceAll(a, str,e, new))
    | NONE => String.extract(str, i, NONE)
                            
end :
sig
    val match : RegexpParser.t * string * int ->  (int * int * ((int * int) array)) option
    val matchString : RegexpParser.t * string * int -> (string * string array) option
    val matchStrings : RegexpParser.t * string * int -> string list
    val doesMatch : RegexpParser.t * string * int -> bool
    val split : RegexpParser.t * string * int -> string list
    val replace : RegexpParser.t * string * int * string-> string
    val replaceAll : RegexpParser.t * string * int * string -> string
end
