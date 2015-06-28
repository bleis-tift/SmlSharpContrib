functor RegexpAST (X: sig
                       eqtype char
                       val chr: int -> char
                   end) : REGEXP_AST =
struct
    type char = X.char
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
    fun fromLiteral c = X.chr (ord c)
end
