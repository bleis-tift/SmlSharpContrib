structure JsonEncoder = struct

  open Std
  open JsonValue

  structure Args = struct
    type t = {
      space : int,
      newLine : string
    }
    val minify = { space = 0, newLine = "" }
    val lf = { space = 0, newLine = "\n" }
    val crlf = { space = 0, newLine = "\r\n" }
    val twoSpaceLf = { space = 2, newLine = "\n" }
    val fourSpaceLf = { space = 4, newLine = "\n" }
    val twoSpaceCrlf = { space = 2, newLine = "\r\n" }
    val fourSpaceCrlf = { space = 4, newLine = "\r\n" }
  end

  fun encode args json =
    let
      val newline = #newLine args
      val space = #space args
      fun cs i = CharVector.tabulate (i, const #" ")
      fun str s = "\"" ^ s ^ "\""
      val colon = if space <> 0 then " : " else ":"
      fun inner i j =
        case j of
          JsonNull => "null"
          | JsonBool b => Bool.toString b
          | JsonNumber n =>
            if n < 0.0 then "-" ^ Real.toString (Real.abs n)
            else Real.toString n
          | JsonString s => str s
          | JsonArray [] => "[]"
          | JsonArray (x::xs) =>
            let
              fun array v =
                "[" ^ newline ^ v ^ newline ^ cs (i - space) ^ "]"
              fun elem v = cs i ^ inner (i + space) v
              val f = elem x
              val s =
                String.concat (List.map (fn v =>
                  "," ^ newline ^ elem v) xs)
            in
              array (f ^ s)
            end
          | JsonObject [] => "{}"
          | JsonObject ((kx, vx)::xs) =>
            let
              fun object v =
                "{" ^ newline ^ v ^ newline ^ cs (i - space) ^ "}"
              fun kv k v = cs i ^ str k ^ colon ^ inner (i + space) v
              val f = kv kx vx
              val s =
                String.concat (List.map (fn (k, v) =>
                  "," ^ newline ^ kv k v) xs)
            in
              object (f ^ s)
            end
    in
      case json of
        JsonObject _ => SOME(inner space json)
        | JsonArray _ => SOME(inner space json)
        | _ => NONE
    end
end

