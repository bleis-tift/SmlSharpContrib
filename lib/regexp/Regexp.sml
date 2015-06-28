functor Regexp(X: sig
                   structure S:  sig
                                 eqtype char
                                        eqtype string
                                 val ^ : string * string -> string
                                 val sub: string * int -> char
                                 val size: string -> int
                                 val implode: char list -> string
                                 val substring: string * int * int -> string
                                 val extract: string * int * int option -> string
                             end
                   structure Matcher : REGEXP_MATCHER
                   sharing type S.string = Matcher.string
                   sharing type S.char = Matcher.char
               end
              ) = 
struct
    open X
    val match = Matcher.match
    val op ^ = S.^
    type t = Matcher.t

    fun doesMatch(r, str, i) =
      case match(r, str, i) of
          SOME _ => true
        | NONE => false

    fun matchString(r, str, i) =
      case match(r, str, i) of
          SOME(s, e, gs) => let
           val gs' = Array.array(Array.length gs, S.implode [])
           val _ = Array.appi (fn(i, (s, e))=> Array.update(gs', i, S.substring(str, s, e - s))) gs
       in
           SOME(S.substring(str, s, e - s), gs')
       end
        | NONE => NONE

    fun matchStrings(r, str, i) =
      case match(r, str, i) of
          SOME(s, e, _) => S.extract(str, s, SOME (e - s)) :: matchStrings(r, str, e)
        | NONE => []

    fun split(a, str, i) =
      case match(a, str, i) of
          SOME(s, e, _) => S.extract(str, i, SOME (s - i)) :: split(a, str, e)
        | NONE => [S.extract(str, i, NONE)]

    fun replace(a, str, i, new) =
      case match(a, str, i) of
          SOME(s, e, _) => S.extract(str, i, SOME (s - i)) ^ new ^ (S.extract(str,e , NONE))
        | NONE =>  str

    fun replaceAll(a, str, i, new) =
      case match(a, str, i) of
          SOME(s, e, _) =>  S.extract(str, i, SOME (s - i)) ^ new ^ (replaceAll(a, str,e, new))
        | NONE => S.extract(str, i, NONE)
                           
end
