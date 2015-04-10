structure Re = 
struct
open Regexp
exception Lex = RegexpParser.Lex
exception Parse = RegexpParser.Parse
type t = RegexpParser.t
fun re str = RegexpParser.re str
end :
sig
    type t
    exception Lex
    exception Parse
    val re : string -> t
    val match : t * string * int ->  (int * int * ((int * int) array)) option
    val matchString : t * string * int -> (string * string array) option
    val matchStrings : t * string * int -> string list
    val doesMatch : t * string * int -> bool
    val split : t * string * int -> string list
    val replace : t * string * int * string-> string
    val replaceAll : t * string * int * string -> string
end
