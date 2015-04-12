functor RegexpParser(X:sig
                          structure C: sig
                                   eqtype char
                                   eqtype string
                                   val ord: char -> int
                                   val chr: int -> char
                                   val minChar: char
                                   val maxChar: char
                                   val notContains: string -> char -> bool
                               end
                     structure S: sig
                                   eqtype char
                                   eqtype string
                                   val toString: string -> String.string
                                   val implode: char list -> string
                                   val explode: string -> char list
                               end
                                   
                     sharing type C.char = S.char
                     sharing type C.string = S.string
                     end
                    )
        : REGEXP_PARSER 
=
struct
open X
type string = C.string
type char = C.char

fun fromLiteral c = C.chr (ord c)
val c = fromLiteral
fun stringToInt (str : string) = Int.fromString (S.toString str)

datatype ast
  = Item of char
  | LineStart
  | LineEnd
  | Or of ast list
  | And of ast list
  | Kleene of ast
  | Group of int * ast
  | Any
       | Empty

type t = ast * int

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
  | LeftBracket
  | RightBrackt
  | LeftBrace
  | RightBrace
  | Hyphen
  | Bang
  | Comma
  | EOS

fun toItem t =
  case t of
      Char x => Item x
    | Hat => Item (c #"^")
    | Dollar => Item (c #"$")
    | Dot => Item (c #".")
    | Star => Item (c #"*")
    | Plus => Item (c #"+")
    | Bar => Item (c #"|")
    | Option => Item (c #"?")
    | LeftParen => Item (c #")")
    | RightParen => Item (c #"(")
    | LeftBracket => Item (c #"[")
    | RightBrackt => Item (c #"]")
    | LeftBrace => Item (c #"{")
    | RightBrace => Item (c #"}")
    | Hyphen => Item (c #"-")
    | Bang => Item (c #"!")
    | Comma => Item (c #",")
    | EOS => Item (c #" ")
    | SOS => Item (c #" ")

fun toChar t =
  case t of
      Char x => Char x
    | Hat => Char (c #"^")
    | Dollar => Char (c #"$")
    | Dot => Char (c #".")
    | Star => Char (c #"*")
    | Plus => Char (c #"+")
    | Bar => Char (c #"|")
    | Option => Char (c #"?")
    | LeftParen => Char (c #")")
    | RightParen => Char (c #"(")
    | LeftBracket => Char (c #"[")
    | RightBrackt => Char (c #"]")
    | LeftBrace => Char (c #"{")
    | RightBrace => Char (c #"}")
    | Hyphen => Char (c #"-")
    | Bang => Char (c #"!")
    | Comma =>  Char (c #",")
    | EOS => Char (c #" ")
    | SOS =>  Char (c #" ")

exception Lex

fun lex str =
  let
      fun loop [] = []
        | loop (c :: cs) = 
          case chr (C.ord c) of
              #"." => Dot :: loop cs
            | #"^" => Hat :: loop cs
            | #"$" => Dollar :: loop cs
            | #"*" => Star :: loop cs
            | #"|" => Bar :: loop cs
            | #"+" => Plus :: loop cs
            | #"?" => Option :: loop cs
            | #"\\" => (case cs of
                            [] => raise Lex
                          | c' :: cs' => Char c' :: loop cs')
            | #"(" => LeftParen :: loop cs
            | #")" => RightParen :: loop cs
            | #"[" => LeftBracket :: loop cs
            | #"]" => RightBrackt :: loop cs
            | #"{" => LeftBrace :: loop cs
            | #"}" => RightBrace :: loop cs
            | #"-" => Hyphen :: loop cs
            | #"!" => Bang :: loop cs
            | #"," => Comma :: loop cs
            | _    => Char c :: loop cs
      fun addSOS (LeftParen, SOS::xs) = SOS::LeftParen::SOS::xs
        | addSOS (x, xs) = x :: xs
      fun addEOS (RightParen, EOS::xs) = EOS::RightParen::EOS::xs
        | addEOS (x, xs) = x :: xs
  in
      List.foldl addEOS [EOS] (List.foldl addSOS [SOS] (loop (S.explode str)))
  end


exception Parse

fun makeRange(Char s, Char e) =
  let
      val si = C.ord s
      val ei = C.ord e
  in
      List.tabulate(ei - si + 1, fn i => Item (C.chr (si + i)))
      handle Size => raise Parse
  end
  | makeRange _ =  raise Parse

fun complementSet xs =
  let
      val whole = List.tabulate((C.ord C.maxChar) - (C.ord C.minChar) + 1 , fn i => (C.chr ((C.ord C.minChar) + i)))
      val str = (S.implode (List.map (fn x => case x of
                                                  Item x' => x'
                                                | _ => raise Parse ) xs))
  in
      List.map (fn x => Item x) (List.filter (fn x => C.notContains str x) whole)
  end

fun parseCharSet [] acc = List.rev acc
  | parseCharSet (SOS :: Hat :: ts) acc = complementSet (parseCharSet (SOS :: ts) acc)
  | parseCharSet (SOS :: RightBrackt :: ts) acc = parseCharSet ts ((toItem RightBrackt) :: acc)
  | parseCharSet (SOS :: Hyphen :: ts) acc = parseCharSet ts ((toItem Hyphen) :: acc)
  | parseCharSet (SOS :: ts) acc = parseCharSet ts acc
  | parseCharSet (Hyphen :: t :: ts) acc = (case acc of
                                                (Item x) :: xs => parseCharSet ts ((makeRange(Char x, (toChar t))) @ xs)
                                              | _ => raise Parse)
  | parseCharSet (t :: ts) acc = parseCharSet ts ((toItem t) :: acc)
                                              

and parse((t :: ts), acc, e, gi) =
    if (case (t, e) of
            (EOS, EOS) => true
          | (RightParen, RightParen) => true
          | _ => false)
    then (And(List.foldl (fn(x, y) => (x :: y)) [] acc), ts, gi)
    else (case t of
              SOS => (case ts of
                          Hat :: ts'=> parse(ts', LineStart :: acc, e, gi)
                        | _ => parse(ts, acc, e, gi))
            | Char c => parse(ts, (Item c) :: acc, e, gi)
            | Dollar => (case ts of
                             EOS :: ts' => parse(EOS :: ts', LineEnd :: acc, e, gi)
                           | _ => parse(ts, (toItem Dollar) :: acc, e, gi))
            | Dot => parse(ts, Any :: acc, e, gi)
            | Star => (case acc of
                           [] => raise Parse
                         | Group(i, x) :: xs  => parse(ts, Group(i, Kleene x) :: xs, e, gi)
                         | x :: xs => parse(ts, Kleene x :: xs, e, gi))
            | Plus => (case acc of
                           [] => raise Parse
                         | Group(i, x) :: xs => parse(ts, Group(i, And(x :: [Kleene x])) :: xs, e, gi)
                         | x :: xs => parse(ts, And(x :: [Kleene x]) :: xs, e, gi))
            | Option => (case acc of
                             [] => raise Parse
                           | Group(i, x) :: xs => parse(ts, Group(i, Or(x :: [Empty])) :: xs, e, gi)
                           | x :: xs => parse(ts, Or(x :: [Empty]) :: xs, e, gi))
            | Bar => (case (parse(ts, [], e, gi), acc) of
                          ((result, rest, gi'), acc') => (Or([And(List.rev acc'), result]), rest, gi'))
            | LeftParen => let val (result, rest, gi') = parse(ts, [], RightParen, gi + 1)
                           in  parse(rest, (Group(gi, result)) :: acc, e, gi') end
            | LeftBracket => let fun collect (RightBrackt :: xs) acc' = (List.rev acc', xs)
                                   | collect (SOS :: RightBrackt :: xs) acc' = collect xs (RightBrackt :: acc')
                                   | collect (SOS :: xs) acc' = collect xs  acc'
                                   | collect (x :: xs) acc' = collect xs (x :: acc')
                                   | collect [] _ = raise Parse
                                 val (charSet, rest) = collect (SOS :: ts) []
                             in
                                 parse(rest, (Or (parseCharSet (SOS :: charSet) []) :: acc), e, gi)
                             end
            | LeftBrace => (let fun collect1 (Comma :: xs) [] l = (xs, NONE)
                                  | collect1 (Comma :: xs) acc' l = (case stringToInt (S.implode (List.rev acc')) of
                                                                         SOME l' =>  (xs, SOME l')
                                                                       | NONE => raise Parse)
                                  | collect1 ((Char x) :: xs) acc' l =  collect1 xs (x :: acc') l
                                  | collect1 _ _ _ = raise Parse
                                fun collect2 (RightBrace :: xs) [] r = (xs, NONE)
                                  | collect2 (RightBrace :: xs) acc' r = (case stringToInt (S.implode (List.rev acc')) of
                                                                              SOME r' => (xs, SOME r')
                                                                            | NONE => raise Parse)
                                  | collect2 ((Char x) :: xs) acc' r =  collect2 xs (x :: acc') r
                                  | collect2 _ _ _ =  raise Parse
                                val (xs, l) = collect1 ts [] NONE
                                val (xs', r) = collect2 xs [] NONE
                            in
                                case (l, r, acc) of
                                    (SOME l', SOME r', Group(i, a) :: acc'') => parse(xs', Group(i, And ((List.tabulate(l', (fn _ => a))) @
                                                                                                         [(And (List.tabulate((r' - l'), (fn _ => Or [a, Empty]))))])) :: acc'', e, gi)
                                  | (SOME l', SOME r', a :: acc'') => parse(xs', (And ((List.tabulate(l', (fn _ => a))) @
                                                                                       [(And (List.tabulate((r' - l'), (fn _ => Or [a, Empty]))))])) :: acc'', e, gi)
                                  | (NONE, SOME r', Group(i, a) :: acc'') => parse(xs', Group(i, And (List.tabulate(r', (fn _ => Or [a, Empty])))) :: acc'', e, gi)
                                  | (NONE, SOME r', a :: acc'') => parse(xs', (And (List.tabulate(r', (fn _ => Or [a, Empty])))) :: acc'', e, gi)
                                  | (SOME l', NONE, Group(i, a) :: acc'') => parse(xs',  Group(i, And (Kleene a :: List.tabulate(l', (fn _ => a)))) :: acc'', e, gi)
                                  | (SOME l', NONE, a :: acc'') => parse(xs', And(Kleene a :: List.tabulate(l', (fn _ => a))) :: acc'', e, gi)
                                  | _ => raise Parse
                            end
                            handle Size => raise Parse)
            | x as Hat => parse(ts, (toItem x) :: acc, e, gi)
            | x as RightParen=> parse(ts, (toItem x) :: acc, e, gi)
            | x as RightBrackt => parse(ts, (toItem x) :: acc, e, gi)
            | x as RightBrace => parse(ts, (toItem x) :: acc, e, gi)
            | x as Comma => parse(ts, (toItem x) :: acc, e, gi)
            | EOS => parse(ts, acc, e, gi)
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
        | f (Group(i, x)) = Group(i, (f x))
        | f x = x
      (* remove Empty *)
      fun c (And xs) = (case (List.filter (fn x => case x of
                                                       Empty => false
                                                     | _ => true)
                                          (List.map c xs)) of
                            [] => Empty
                          | [x] => x
                          | xs' => And xs')
        | c (Or xs) = Or (List.map c xs)
        | c (Kleene x) = (case c x of
                              Empty => Empty
                            | x' => Kleene x')
        | c (Group(i, x)) = Group(i, (c x))
        | c x =  x
  in
      c (f a)
  end
      

fun re str =
  let val (res, _, gi) = parse(lex str, [], EOS, 0) in (compaction res, gi) end
end
