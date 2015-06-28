signature REGEXP_MATCHER =
sig
    eqtype char
    eqtype string
    type t
    val match: t * string * int -> (int * int * ((int * int) array)) option
end
