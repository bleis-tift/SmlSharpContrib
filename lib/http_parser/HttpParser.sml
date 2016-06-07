structure HttpParser =
struct


type chunkedDecoder = unit ptr
type request = {method: Substring.substring, path: Substring.substring, minorVersion: int, headers: (Substring.substring * Substring.substring) list, parsedSize: int}
type response = {minorVersion: int, status: int, message: Substring.substring, headers: (Substring.substring * Substring.substring) list, parsedSize: int}
type headers = {headers: (Substring.substring * Substring.substring) list, parsedSize: int}


exception Parse


val c_parse_request = _import "http_parser_parse_request":
                      (string, int,
                       int ref, int ref,
                       int ref, int ref,
                       int ref, (int, int, int, int) -> ()) -> int

val c_parse_response = _import "http_parser_parse_response":
                       (string, int,
                        int ref, int ref,
                        int ref, int ref,
                        (int, int, int, int) -> ()) -> int

val c_parse_headers = _import "http_parser_parse_headers":
                      (string, int,
                       (int, int, int, int) -> ()) -> int

val c_decoder = _import "http_parser_decoder": () -> chunkedDecoder
val c_decode_chunked = _import "http_parser_decode_chunked": (chunkedDecoder, char array, int , int ref) -> int

val subs = Substring.substring

fun parseRequestString str = let
    val len = String.size(str)
    val methodStart  = ref 0
    val methodSize   = ref 0
    val pathStart    = ref 0
    val pathSize     = ref 0
    val minorVersion = ref 0
    val headers       = ref []
    fun callback (n, nsz, v, vsz) =
      if n = ~1
      then case !headers of
               (name, value)::tl =>
               headers := (name, Substring.full((Substring.string value)
                                                ^ (String.substring(str, v, vsz)))) :: tl
            | _ => raise Fail "unreachable"
      else headers := (subs(str, n, nsz), subs(str, v, vsz)) :: (!headers)
    val ret = c_parse_request(str, len,
                              methodStart, methodSize,
                              pathStart, pathSize,
                              minorVersion, callback)
in
    case ret of
        ~1 => raise Parse
      | ~2 => NONE
      | parsedSize => SOME ({
                               method       = subs(str, !methodStart, !methodSize),
                               path         = subs(str, !pathStart,   !pathSize),
                               minorVersion = !minorVersion,
                               headers      = List.rev (!headers),
                               parsedSize   = parsedSize
                           })
end

fun parseResponseString str = let
    val len = String.size(str)
    val minorVersion = ref 0
    val status = ref 0
    val msgStart  = ref 0
    val msgSize   = ref 0
    val headers       = ref []
    fun callback (n, nsz, v, vsz) =
      if n = ~1
      then case !headers of
               (name, value)::tl =>
               headers := (name, Substring.full((Substring.string value)
                                                ^ (String.substring(str, v, vsz)))) :: tl
             | _ => raise Fail "unreachable"
      else headers := (subs(str, n, nsz), subs(str, v, vsz)) :: (!headers)
    val ret = c_parse_response(str, len, minorVersion, status,
                               msgStart, msgSize, callback)
in
    case ret of
        ~1 => raise Parse
      | ~2 => NONE
      | parsedSize => SOME ({
                               message  = subs(str, !msgStart, !msgSize),
                               status = !status,
                               minorVersion = !minorVersion,
                               headers      = List.rev (!headers),
                               parsedSize   = parsedSize
                           })
end

fun parseHeadersString str = let
    val len = String.size(str)
    val headers       = ref []
    fun callback (n, nsz, v, vsz) =
      if n = ~1
      then case !headers of
               (name, value)::tl =>
               headers := (name, Substring.full((Substring.string value)
                                                ^ (String.substring(str, v, vsz)))) :: tl
             | _ => raise Fail "unreachable"
      else headers := (subs(str, n, nsz), subs(str, v, vsz)) :: (!headers)
    val ret = c_parse_headers(str, len,  callback)
in
    case ret of
        ~1 => raise Parse
      | ~2 => NONE
      | parsedSize => SOME ({
                               headers      = List.rev (!headers),
                               parsedSize   = parsedSize
                           })
end


val decoder = c_decoder

fun decodeChunkedCharArray decoder buf start size =
  case c_decode_chunked(decoder, buf, start, size) of
      ~1 => raise Parse
    | ~2 => NONE
    | x  => SOME(x)


end
