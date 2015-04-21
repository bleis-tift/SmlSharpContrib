functor Regexp(X: sig
                   structure S:  sig
                   eqtype char
                   eqtype string
                   val ^ : string * string -> string
                   val sub: string * int -> char
                   val size: string -> int
                   val implode: char list -> string
                   val substring: string * int * int -> string
                   val extract: string * int * int option -> string
                   end
                   structure Parser: REGEXP_PARSER
                   sharing type S.char = Parser.char
                   sharing type S.string = Parser.string
               end
              ) = 
struct
open X
open Parser
val op ^ = S.^

datatype result
  = Success of int * int * ((int * int) array)
  | Continue
  | Fail
        

fun match_aux(r, rest, str, i, gs) =
  case r of
      Item c => ((if S.sub(str, i) = c
                  then Success(i, i+1, gs)
                  else Continue)
                 handle Subscript => Fail)
    | LineStart => ((if i = 0  orelse S.sub(str, i - 1) = (Parser.fromLiteral #"\n")
                     then Success(i, i, gs)
                     else Continue)
                    handle Subscript => Fail)
    | LineEnd => ((if i = (S.size str) orelse S.sub(str, i) = (Parser.fromLiteral #"\n")
                   then Success(i, i, gs)
                   else Continue)
                  handle Subscript => Fail)
    | Or [] => if i = (S.size str)
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
    | Any => ((if 0 <= i andalso (i < S.size str)
               then Success(i, i+1, gs)
               else Continue)
              handle Subscript => Fail)
    | Empty => Success(i, i, gs)

fun match(r : t as (ast, gi), str, i) =
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
       val gs' = Array.array(Array.length gs, S.implode [])
       val _ = Array.appi (fn(i, (s, e))=> Array.update(gs', i, S.substring(str, s, e - s))) gs
   in
       SOME(S.substring(str, s, e - s), gs')
   end
    | NONE => NONE

fun matchStrings(r, str, i) =
  case match(r, str, i) of
      SOME(s, e, _) => S.extract(str, s, SOME (e - s)) :: matchStrings(r, str, e)
    | NONE => []

fun split(a, str, i) =
  case match(a, str, i) of
      SOME(s, e, _) => S.extract(str, i, SOME (s - i)) :: split(a, str, e)
    | NONE => [S.extract(str, i, NONE)]

fun replace(a, str, i, new) =
  case match(a, str, i) of
      SOME(s, e, _) => S.extract(str, i, SOME (s - i)) ^ new ^ (S.extract(str,e , NONE))
    | NONE =>  str

fun replaceAll(a, str, i, new) =
  case match(a, str, i) of
      SOME(s, e, _) =>  S.extract(str, i, SOME (s - i)) ^ new ^ (replaceAll(a, str,e, new))
    | NONE => S.extract(str, i, NONE)
                       
end :
sig
    type t
    type string
    exception Lex
    exception Parse
    val re : string -> t
    val match : t * string * int ->  (int * int * ((int * int) array)) option
    val matchString : t * string * int -> (string * string array) option
    val matchStrings : t * string * int -> string list
    val doesMatch : t * string * int -> bool
    val split : t * string * int -> string list
    val replace : t * string * int * string-> string
    val replaceAll : t * string * int * string -> string
end
