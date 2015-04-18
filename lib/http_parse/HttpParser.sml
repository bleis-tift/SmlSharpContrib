structure HttpParser =
struct

type headers = unit ptr
type headerArray = (headers * int)
exception Parse
exception MemoryFull
exception Incomplete

val phr_prepare_headers = _import "phr_prepare_headers": __attribute__((no_callback))
                                                                      int -> headers

val phr_header_at = _import "phr_header_at": __attribute__((no_callback))
                                                          (headers, int, string ref, int ref, string ref, int ref) -> ()

val phr_parse_request =
    _import "phr_parse_request":  __attribute__((no_callback))
                                               (
                                                 string, int,
                                                 string ref, int ref,
                                                 string ref, int ref,
                                                 int ref,
                                                 headers, int ref,
                                                 int
                                               ) -> int

val phr_parse_response =
    _import "phr_parse_response": __attribute__((no_callback))
                                               (
                                                 string, int,
                                                 int ref,
                                                 int ref, string ref, int ref,
                                                 headers, int ref,
                                                 int
                                               ) -> int

fun prepareHeaders n =
  let
      val headers = phr_prepare_headers n
  in
      if headers = _NULL
      then raise MemoryFull
      else (headers, n)
  end

fun getHeader (headers, n) i =
  let
      val name = ref ""
      val nameLen = ref 0
      val value = ref ""
      val valueLen = ref 0
  in
      if 0 <= i andalso i < n
      then phr_header_at(headers, i,  name, nameLen, value, valueLen)
      else raise Subscript
    ;
      if (!name) = ""
      then (NONE, String.substring(!value, 0, !valueLen))
      else (SOME(String.substring(!name, 0, !nameLen)), String.substring(!value, 0, !valueLen))
  end

fun getHeaders headers n =
  let
      fun loop i acc =
        if i = n
        then acc
        else loop (i + 1) ((getHeader headers i) :: acc)

      fun maybeAppend str1 (SOME str2) = str1 ^ str2
        | maybeAppend str1 (NONE) = str1
  in
      #1 (List.foldl (fn ((name, value), (acc, value_acc))=>
                         case name of
                             NONE => (acc, SOME(maybeAppend value value_acc))
                           | SOME name' => ((name', maybeAppend value value_acc):: acc, NONE))
                     ([], NONE) (loop 0 []))
  end
      
fun parseRequest (headerArray as (headers, n)) buf =
  let
      val bufLen = String.size(buf)
      val method = ref ""
      val methodLen = ref 0
      val path = ref ""
      val pathLen = ref 0
      val minorVersion = ref 0
      val numHeaders = ref n
  in
      case phr_parse_request(buf, bufLen, method, methodLen, path, pathLen,
                             minorVersion, headers, numHeaders, 0) of
          ~1 => raise Parse
        | ~2 => raise Incomplete
        | x => (
            String.substring(!method, 0, !methodLen),
            String.substring(!path, 0, !pathLen),
            !minorVersion,
            getHeaders (headers, n) (!numHeaders)
        )
  end

fun parseResponse (headerArray as (headers, n)) buf =
  let
      val bufLen = String.size(buf)
      val minorVersion = ref 0
      val status = ref 0
      val msg = ref ""
      val msgLen = ref 0
      val numHeaders = ref n
  in
      case phr_parse_response(buf, bufLen, minorVersion, status, msg, msgLen,
                              headers, numHeaders, 0) of
          ~1 => raise Parse
        | ~2 => raise Incomplete
        | x  => (
            !minorVersion, !status,
            String.substring(!msg, 0, !msgLen),
            getHeaders headerArray (!numHeaders)
        )
  end
end
