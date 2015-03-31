structure Re = 
struct
datatype t
  = Item of char
  | Or of t * t
  | And of t * t
  | Kleene of t
  | Any
  | Empty
        
datatype token
  = Char of char
  | Dot
  | Star
  | Plus
  | Bar
  | Option
  | LeftParen
  | RightParen
  | EOF

exception Lex

fun lex str =
  let
      fun loop [] = [EOF]
        | loop (c :: cs) = 
          case c of
              #"." => Dot :: loop cs
            | #"*" => Star :: loop cs
            | #"|" => Bar :: loop cs
            | #"+" => Plus :: loop cs
            | #"?" => Option :: loop cs
            | #"\\" => (case cs of
                            [] => raise Lex
                          | c' :: cs' =>  Char c' :: loop cs')
            | #"(" => LeftParen :: loop cs
            | #")" => RightParen :: loop cs
            | _  => Char c :: loop cs
                                   
  in
      loop (String.explode str)
  end


exception Parse

fun parse_one [] = NONE
  | parse_one((t :: ts) : token list) =
    case t of
        Char c => SOME(Item c, ts)
      | LeftParen => SOME(parse(ts, [], RightParen))
      | _ => NONE              
and parse([], acc, EOF)  = (List.foldl (fn(x, y) => And(x, y)) Empty acc, [])
  | parse((t :: ts), acc, e) =
    if t = e
    then (List.foldl (fn(x, y) => And(x, y)) Empty acc, ts)
    else (case t of
              Char c =>  parse(ts, (Item c) :: acc, e)
            | Dot => parse(ts, Any :: acc, e)
            | Star => (case acc of
                           [] => raise Parse
                         | x :: xs => parse(ts, Kleene x :: xs, e))
            | Plus => (case acc of
                           [] => raise Parse
                         | x :: xs => parse(ts, And(x,Kleene x) :: xs, e))
            | Option => (case acc of
                             [] => raise Parse
                           | x :: xs => parse(ts, Or(x, Empty) :: xs, e))
            | Bar => (case (parse_one ts, acc) of
                          (SOME(result, rest), x :: xs) => parse(rest, Or(x, result) :: xs, e)
                        | _ => raise Parse)
            | LeftParen => let val (result, rest) = parse(ts, [], RightParen)
                           in  parse(rest, result :: acc, e) end
            | _ =>  raise Parse)
  | parse _ =  raise Parse

(* Not for performance but for pretty printing *)
fun compaction a =
  let
      fun c (And(x, Empty)) = c x
        | c (And(x, y)) =
          let
              val x' = c x
              val y' = c y
          in
              if x' = Empty
              then y'
              else if y' = Empty
              then x'
              else And(x', y')
          end
        | c (Or(x, y)) = if x = y
                         then c x
                         else Or(c x, c y)
        | c (Kleene Empty) = Empty
        | c (Kleene x) = Kleene(c x)
        | c x =  x
  in
      c a
  end
      

fun re str =
  let val (res, _) = parse(lex str, [], EOF) in compaction res end


datatype result
  = Success of int * int
  | Continue
  | Fail
        

fun match_aux(r, rest, str, i) =
  case r of
      Item c => ((if String.sub(str, i) = c
                  then Success(i, i+1)
                  else Continue)
                 handle Subscript => Fail)
    | Or(x, y) => (case match_aux(x, rest, str, i) of
                       Success(s, e) => (case match_aux(rest, Empty, str, e) of (* backtrack *)
                                             Success _ => Success(s, e)
                                           | Continue => match_aux(y, rest, str, i)
                                           | Fail => match_aux(y, rest, str, i) )
                     | Continue => match_aux(y, rest, str, i)
                     | Fail => match_aux(y, rest, str, i))
    | And(x, y) => (case (case match_aux(x, y, str, i) of
                              Success(_, e) => match_aux(y, rest, str, e)
                            | Continue => Continue
                            | Fail => Fail) of
                        Success(_, e) => Success(i, e)
                      | Continue => Continue
                      | Fail => Fail)
    | Kleene x => (case match_aux(x, rest, str, i) of
                       Success(_, e) =>
                       (case match_aux(rest, Empty, str, e) of (* for backtrack *)
                            Success _ =>   (* can backtrack *)
                            (case match_aux(r, rest, str, e) of (* try procede *)
                                 Success(_, e') => Success(i, e')
                               | _ => Success(i, e))
                          | _ => (* need procede or backtrack *)
                            (case match_aux(r, rest, str, e) of
                                 Success(_, e') => Success(i, e')
                               | _ => (case match_aux(rest, Empty, str, i) of
                                           Success _ => Success(i, i)
                                         | Continue => Continue
                                         | Fail => Fail)))
                     | _ => (case match_aux(rest, Empty, str, i) of
                                 Success _ => Success(i, i)
                               | Continue => Continue
                               | Fail => Fail))
    | Any => ((if 0 <= i andalso (i < String.size str)
                  then Success(i, i+1)
                  else Continue)
                 handle Subscript => Fail)
    | Empty => Success(i, i)      

fun match(r, str, i) =
  case match_aux(r, Empty, str, i) of
      Success(s, e) => SOME(s, e)
    | Continue => match(r, str, i + 1)
    | Fail => NONE

fun doesMatch(r, str, i) =
  case match(r, str, i) of
      SOME _ => true
    | NONE => false

fun matchString(r, str, i) =
  case match(r, str, i) of
      SOME(s, e) => SOME(String.substring(str, s, e - s), [])
    | NONE => NONE

fun matchStrings(r, str, i) =
  case match(r, str, i) of
      SOME(s, e) => String.extract(str, s, SOME (e - s)) :: matchStrings(r, str, e)
    | NONE => []

fun split(a, str, i) =
  case match(a, str, i) of
      SOME(s, e) => String.extract(str, i, SOME (s - i)) :: split(a, str, e)
    | NONE => [String.extract(str, i, NONE)]

fun replace(a, str, i, new) =
  case match(a, str, i) of
      SOME(s, e) => String.extract(str, i, SOME (s - i)) ^ new ^ (String.extract(str,e , NONE))
    | NONE =>  str

fun replaceAll(a, str, i, new) =
  case match(a, str, i) of
      SOME(s, e) =>  String.extract(str, i, SOME (s - i)) ^ new ^ (replaceAll(a, str,e, new))
    | NONE => String.extract(str, i, NONE)
                            
end :
sig
    type t
    exception Lex
    exception Parse
    val re : string -> t
    val match : t * string * int ->  (int * int) option
    val matchString : t * string * int -> (string * string list) option
    val matchStrings : t * string * int -> string list
    val doesMatch : t * string * int -> bool
    val split : t * string * int -> string list
    val replace : t * string * int * string-> string
    val replaceAll : t * string * int * string -> string
end
