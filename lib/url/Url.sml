structure Url =
struct
exception Invalid
fun toHex w = String.sub("0123456789ABCDEF",(Word8.toInt w))
fun fromHex c = Word8.fromInt(if Char.isDigit c
                              then (ord c) - (ord #"0")
                              else (ord (Char.toLower c) - (ord #"a")) + 10)
fun toChar w = chr(Word8.toInt w)
val urlEncode = String.translate (fn x => case x of
                                              #"-" => "-"
                                            | #"_" => "_"
                                            | #"." => "."
                                            | #"~" => "~"
                                            | #" " => "+"
                                            | c => let val c' = Word8.fromInt(ord c)
                                                   in String.implode [#"%", toHex(Word8.>>(c', 0w4)), toHex(Word8.andb(c', 0w15))]
                                                   end)

fun urlDecode str =
  let
      fun loop [] acc = List.rev acc
        | loop (x::xs) acc = case x of
                                 #"+" => loop xs (#" "::acc)
                               | #"%" => (case xs of
                                              a::b::xs' => loop xs' (toChar(Word8.orb(Word8.<<(fromHex(a), 0w4), fromHex(b)))::acc)
                                           | _ => raise Invalid)
                               | _ => loop xs (x::acc)
  in
      String.implode (loop (String.explode str) [])
  end
end
