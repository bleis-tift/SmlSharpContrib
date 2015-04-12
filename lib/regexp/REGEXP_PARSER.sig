signature REGEXP_PARSER =
sig
    (* structure C: CHAR *)
    (* structure S: STRING *)
    (* sharing type C.char = S.char *)
    (* sharing type C.string = S.string *)
    eqtype string
           eqtype char
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
    exception Lex
    exception Parse
    val fromLiteral: Char.char -> char
    val re : string -> t
end
