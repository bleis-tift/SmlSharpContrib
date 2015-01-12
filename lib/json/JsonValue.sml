structure JsonValue = struct
  datatype t =
    JsonNull
    | JsonBool of bool
    | JsonNumber of real
    | JsonString of string
    | JsonArray of t array
    | JsonObject of (string * t) list
end

