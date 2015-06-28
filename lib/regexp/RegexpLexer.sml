functor RegexpLexer(X:sig
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
                     end) =
struct
    open X
    type string = C.string
    type char = C.char
    fun fromLiteral c = C.chr (ord c)
    val c = fromLiteral
    fun stringToInt (str : string) = Int.fromString (S.toString str)

    datatype t
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


end
