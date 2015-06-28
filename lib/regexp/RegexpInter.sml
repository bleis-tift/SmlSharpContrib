functor RegexpInter(X: sig
                   structure S:  sig
                   eqtype char
                   eqtype string
                   val sub: string * int -> char
                   val size: string -> int
                   end
                   structure AST: REGEXP_AST
                   sharing type S.char = AST.char
               end
              ) = 
struct
open X
open AST
type string = S.string
type t = AST.t * int
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
    | LineStart => ((if i = 0  orelse S.sub(str, i - 1) = (AST.fromLiteral #"\n")
                     then Success(i, i, gs)
                     else Continue)
                    handle Subscript => Fail)
    | LineEnd => ((if i = (S.size str) orelse S.sub(str, i) = (AST.fromLiteral #"\n")
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
end
