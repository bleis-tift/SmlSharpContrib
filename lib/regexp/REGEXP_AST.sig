signature REGEXP_AST=
sig
    eqtype char
    datatype t
      = Any
      | Empty
      | Item of char
      | LineStart
      | LineEnd
      | Or of t list
      | And of t list
      | Kleene of t
      | Group of int * t
    val fromLiteral: Char.char -> char
end
