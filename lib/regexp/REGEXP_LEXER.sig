signature REGEXP_LEXER =
sig
    eqtype char
    eqtype string
    exception Lex
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
    val toChar: t -> t
    val lex: string -> t list
end
