structure Json = struct

  type t = JsonValue.t

  structure Args = struct
    open JsonEncoder
    type t = Args.t
    val minify = Args.minify
    val lf = Args.lf
    val crlf = Args.crlf
    val twoSpaceLf = Args.twoSpaceLf
    val fourSpaceLf = Args.fourSpaceLf
    val twoSpaceCrlf = Args.twoSpaceCrlf
    val fourSpaceCrlf = Args.fourSpaceCrlf
  end

  fun decode input = JsonDecoder.decode input
  fun equal (l, r) = JsonValue.equal (l, r)
  fun encode args json = JsonEncoder.encode args json
end

