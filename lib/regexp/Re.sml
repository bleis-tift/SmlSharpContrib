structure Re = 
struct
datatype t
  = Item of char
  | LineStart
  | LineEnd
  | Or of t list
  | And of t list
  | Kleene of t
  | Any
  | Empty
        
datatype token
  = SOS
  | Char of char
  | Hat
  | Dollar
  | Dot
  | Star
  | Plus
  | Bar
  | Option
  | LeftParen
  | RightParen
  | EOS

exception Lex

fun lex str =
  let
      fun loop [] = [EOS]
        | loop (c :: cs) = 
          case c of
              #"." => Dot :: loop cs
            | #"^" => Hat :: loop cs
            | #"$" => Dollar :: loop cs
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
      SOS :: (loop (String.explode str))
  end


exception Parse

fun parse_one [] = NONE
  | parse_one((t :: ts) : token list) =
    case t of
        Char c => SOME(Item c, ts)
      | LeftParen => SOME(parse(ts, [], RightParen))
      | _ => NONE              
and parse((t :: ts), acc, e) =
    if t = e
    then (And(List.foldl (fn(x, y) => (x :: y)) [] acc), ts)
    else (case t of
              SOS => (case ts of
                          Hat :: ts'=> parse(ts', LineStart :: acc, e)
                        | _ => parse(ts, acc, e))
            | Char c => parse(ts, (Item c) :: acc, e)
            | Dollar => (case ts of
                             [EOS] => parse(ts, LineEnd :: acc, e)
                           | _ => parse(ts, (Item #"$") :: acc, e))
            | Hat => parse(ts, (Item #"^") :: acc, e)
            | Dot => parse(ts, Any :: acc, e)
            | Star => (case acc of
                           [] => raise Parse
                         | x :: xs => parse(ts, Kleene x :: xs, e))
            | Plus => (case acc of
                           [] => raise Parse
                         | x :: xs => parse(ts, And(x :: [Kleene x]) :: xs, e))
            | Option => (case acc of
                             [] => raise Parse
                           | x :: xs => parse(ts, Or(x :: [Empty]) :: xs, e))
            | Bar => (case (parse_one ts, acc) of
                          (SOME(result, rest), x :: xs) => parse(rest, Or(x :: [result]) :: xs, e)
                        | _ => raise Parse)
            | LeftParen => let val (result, rest) = parse(ts, [], RightParen)
                           in  parse(rest, result :: acc, e) end
            | _ =>  raise Parse)
  | parse _ =  raise Parse

(* Not for performance but for pretty printing *)
fun compaction a =
  let
      (* flatten like And [And [a, b], c ] -> And [a, b, c] *)
      fun f (And xs) = And (List.foldr (fn (x, y) =>
                                    let val x' = f x in
                                        case x' of
                                            And xs' => xs' @ y
                                         |  _ => x' :: y
                                    end) [] xs)
        | f (Or xs) = Or (List.foldr (fn (x, y) =>
                                    let val x' = f x in
                                        case x' of
                                            Or xs' => xs' @ y
                                         |  _ => x' :: y
                                    end) [] xs)
        | f (Kleene x) = Kleene (f x)
        | f x = x
      fun c (And xs) = (case (List.filter (fn x => x = Empty) (List.map c xs)) of
                            [] => Empty
                          | xs' => And xs')
        | c (Or xs) = Or (List.map c xs)
        | c (Kleene x) = (case c x of
                              Empty => Empty
                            | x' => Kleene x')
        | c x =  x
  in
      a
  end
      

fun re str =
  let val (res, _) = parse(lex str, [], EOS) in compaction res end


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
    | LineStart => ((if i = 0  orelse String.sub(str, i - 1) = #"\n"
                     then Success(i, i)
                     else Continue)
                    handle Subscript => Fail)
    | LineEnd => ((if i = (String.size str) orelse String.sub(str, i + 1) = #"\n"
                   then Success(i, i)
                   else Continue)
                  handle Subscript => Fail)
    | Or [] => Continue
    | Or (x :: xs) => (case match_aux(x, rest, str, i) of
                       Success(s, e) => (case match_aux(rest, Empty, str, e) of (* backtrack *)
                                             Success _ => Success(s, e)
                                           | Continue => match_aux(Or xs, rest, str, i)
                                           | Fail => match_aux(Or xs, rest, str, i) )
                     | Continue => match_aux(Or xs, rest, str, i)
                     | Fail => match_aux(Or xs, rest, str, i))
    | And [] => Success(i, i)
    | And (x :: xs) => (case (case match_aux(x, And xs, str, i) of
                              Success(_, e) => match_aux(And xs, rest, str, e)
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
