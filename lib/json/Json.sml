structure Json = struct
  type t = JsonValue.t
  fun decode input = JsonDecoder.decode input
  fun equal (l, r) = JsonValue.equal (l, r)
end

