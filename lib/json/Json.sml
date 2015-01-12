structure Json = struct
  type t = JsonValue.t
  fun decode input = JsonDecoder.decode input
end

