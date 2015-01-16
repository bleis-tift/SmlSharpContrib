structure JsonValue = struct
  datatype t =
    JsonNull
    | JsonBool of bool
    | JsonNumber of real
    | JsonString of string
    | JsonArray of t array
    | JsonObject of (string * t) list
  fun equal (JsonNull, JsonNull) = true
    | equal (JsonBool l, JsonBool r) = l = r
    | equal (JsonNumber l, JsonNumber r) =
      (case (Real.isNan l, Real.isNan r) of
        (true, true) => true
        | (false, false) => Real.toString l = Real.toString r
        | _ => false)
    | equal (JsonString l, JsonString r) = l = r
    | equal (JsonArray l, JsonArray r) =
      if (Array.length l) <> (Array.length r) then false
      else
        Array.foldli (fn (i, lv, b) =>
          b andalso equal (lv, Array.sub (r, i))) true l
    | equal (JsonObject l, JsonObject r) =
      if (List.length l) <> (List.length r) then false
      else
        #2 (List.foldl (fn ((lk, lv), (i, b)) =>
          let
            val (rk, rv) = List.nth (r, i)
          in
            (i + 1, b andalso lk = rk andalso equal (lv, rv))
          end) (0, true) l)
    | equal (_, _) = false
end

