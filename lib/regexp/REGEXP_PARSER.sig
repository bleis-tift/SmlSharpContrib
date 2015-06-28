signature REGEXP_PARSER =
sig
    eqtype string
           eqtype char
    exception Parse
    type t
    val fromLiteral: Char.char -> char
    val re : string -> t
end
