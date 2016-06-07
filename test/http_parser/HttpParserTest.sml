structure HttpParserTest = struct
open SMLUnit
open HttpParser
open Assert

val hToString = List.map (fn (n, v) => (Substring.string n, Substring.string v))

fun assertEqualHeader expected actual =
  assertEqualList(assertEqual2Tuple(assertEqualString,
                                    assertEqualString))
                 expected
                 (hToString actual)

fun assertHeaderParsed expected actual =
  (assertSome actual;
   assertEqualHeader (#headers expected) (#headers (Option.valOf actual)))

fun assertRequestParsed expected actual =
  (assertSome actual;
   let
       val a = Option.valOf actual
       val e = expected
   in
       assertEqualString (#method e)       (Substring.string(#method a));
       assertEqualString (#path e)         (Substring.string(#path a));
       assertEqualInt    (#minorVersion e) (#minorVersion a);
       assertEqualHeader (#headers e)      (#headers a)
   end
  )

fun assertResponseParsed expected actual =
  (assertSome actual;
   let
       val a = (Option.valOf actual)
       val e = expected
   in
       assertEqualInt (#minorVersion e) (#minorVersion a);
       assertEqualInt (#status e) (#status a);
       assertEqualString (#message e) (Substring.string (#message a));
       assertEqualHeader (#headers e) (#headers a)
   end
  )

fun assertRequestParseError thunk =
  let val  _ = thunk () in fail "exception didn't occured" end
  handle x => assertEqualExceptionName Parse x

fun assertResponseParseError thunk =
  let val  _ = thunk () in fail "exception didn't occured" end
  handle x => assertEqualExceptionName Parse x

fun assertHeadersParseError thunk =
  let val  _ = thunk () in fail "exception didn't occured" end
  handle x => assertEqualExceptionName Parse x


fun assertRequestIncomplete actual = assertNone actual
fun assertResponseIncomplete actual = assertNone actual
fun assertHeadersIncomplete actual = assertNone actual


val requestTests = [
    ("simple",
     fn () => assertRequestParsed
                  {method = "GET", path = "/", minorVersion = 0, headers = []}
                  (parseRequestString "GET / HTTP/1.0\r\n\r\n")),
    ("partial",
     fn () => assertRequestIncomplete (parseRequestString "GET / HTTP/1.0\r\n\r")),
    ("parse headers",
     fn () => assertRequestParsed
                  {method = "GET", path = "/hoge", minorVersion = 1, headers = [("Host", "example.com"), ("Cookie", "")]}
                  (parseRequestString "GET /hoge HTTP/1.1\r\nHost: example.com\r\nCookie: \r\n\r\n")),
    (* ("multibyte included", *)
    (*  fn () => assertRequestParsed *)
    (*              {method = "GET", path = "/hoge", minorVersion = 1, headers = [("Host", "example.com"), ("User-Agent", "\343\201\262\343/1.0")]} *)
    (*              (parseRequest' "GET /hoge HTTP/1.1\r\nHost: example.com\r\nUser-Agent: \343\201\262\343/1.0\r\n\r\n")), *)
    ("parse multiline",
     fn () => assertRequestParsed
                  {method = "GET", path = "/", minorVersion = 0, headers = [("foo", ""), ("foo", "b  \tc")]}
                  (parseRequestString "GET / HTTP/1.0\r\nfoo: \r\nfoo: b\r\n  \tc\r\n\r\n")),
    ("http header name with trailing space",
     fn () => assertRequestParsed
                  {method = "GET", path = "/", minorVersion = 0, headers = [("foo ", "ab")]}
                  (parseRequestString "GET / HTTP/1.0\r\nfoo : ab\r\n\r\n")),
    ("incomplete 1",
     fn () => assertRequestIncomplete (parseRequestString "GET")),
    ("incomplete 2",
     fn () => assertRequestIncomplete (parseRequestString "GET ")),
    ("incomplete 3",
     fn () => assertRequestIncomplete (parseRequestString "GET /")),
    ("incomplete 4",
     fn () => assertRequestIncomplete (parseRequestString "GET / ")),
    ("incomplete 5",
     fn () => assertRequestIncomplete (parseRequestString "GET / H")),
    ("incomplete 6",
     fn () => assertRequestIncomplete (parseRequestString "GET / HTTP/1.")),
    ("incomplete 7",
     fn () => assertRequestIncomplete (parseRequestString "GET / HTTP/1.0")),
    ("incomplete 8",
     fn () => assertRequestIncomplete (parseRequestString "GET / HTTP/1.0\r")),
    ("slowloris (incomplete)",
     fn () => assertRequestIncomplete (parseRequestString "GET /hoge HTTP/1.0\r\n\r")),
    ("slowloris (complete)",
     fn () => assertRequestParsed
                  {method = "GET", path = "/hoge", minorVersion = 0, headers = []}
                  (parseRequestString "GET /hoge HTTP/1.0\r\n\r\n")),
    ("empty header name",
     fn () => assertRequestParseError (fn () => parseRequestString "GET / HTTP/1.0\r\n:a\r\n\r\n")),
    ("header name (space only)",
     fn () => assertRequestParseError (fn () => parseRequestString "GET / HTTP/1.0\r\n :a\r\n\r\n")),
    ("NUL in method",
     fn () => assertRequestParseError (fn () => parseRequestString "G\000T / HTTP/1.0\r\n\r\n")),
    ("tab in method",
     fn () => assertRequestParseError (fn () => parseRequestString "G\tT / HTTP/1.0\r\n\r\n")),
    ("DEL in uri-path",
     fn () => assertRequestParseError (fn () => parseRequestString "GET /\127hello HTTP/1.0\r\n\r\n")),
    ("NUL in header name",
     fn () => assertRequestParseError (fn () => parseRequestString "GET / HTTP/1.0\r\na\000b: c\r\n\r\n")),
    ("NUL in header value",
     fn () => assertRequestParseError (fn () => parseRequestString "GET / HTTP/1.0\r\nab: c\000d\r\n\r\n")),
    ("CTL in header name",
     fn () => assertRequestParseError (fn () => parseRequestString "GET / HTTP/1.0\r\na\027b: c\r\n\r\n")),
    ("CTL in header value",
     fn () => assertRequestParseError (fn () => parseRequestString "GET / HTTP/1.0\r\nab: c\027\r\n\r\n")),
    ("accept MSB chars",
     fn () => assertRequestParsed
                  {method = "GET", path = "/\160", minorVersion = 0, headers = [("h", "c\162y")]}
                  (parseRequestString "GET /\160 HTTP/1.0\r\nh: c\162y\r\n\r\n"))
]

val responseTests = [
    ("simple",
     fn () => assertResponseParsed
                  {minorVersion = 0, status = 200, message = "OK", headers = []}
                  (parseResponseString "HTTP/1.0 200 OK\r\n\r\n")),
    ("partial",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.0 200 OK\r\n\r")),
    ("parse headers",
     fn () => assertResponseParsed
                  {minorVersion = 1, status = 200, message = "OK", headers = [("Host", "example.com"), ("Cookie", "")]}
                  (parseResponseString"HTTP/1.1 200 OK\r\nHost: example.com\r\nCookie: \r\n\r\n")),
    ("parse multiline",
     fn () => assertResponseParsed
                  {minorVersion = 0, status = 200, message = "OK", headers = [("foo", ""), ("foo", "b  \tc")]}
                  (parseResponseString "HTTP/1.0 200 OK\r\nfoo: \r\nfoo: b\r\n  \tc\r\n\r\n")),
    ("internal server error",
     fn () => assertResponseParsed
                  {minorVersion = 0, status = 500, message = "Internal Server Error", headers = []}
                  (parseResponseString "HTTP/1.0 500 Internal Server Error\r\n\r\n")),
    ("incomplete 1",
     fn () => assertResponseIncomplete (parseResponseString "H")),
    ("incomplete 2",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.")),
    ("incomplete 3",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.1")),
    ("incomplete 4",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.1 ")),
    ("incomplete 5",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.1 2")),
    ("incomplete 6",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.1 200")),
    ("incomplete 7",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.1 200 ")),
    ("incomplete 8",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.1 200 O")),
    ("incomplete 9",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.1 200 OK\r")),
    ("incomplete 10",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.1 200 OK\r\n")),
    ("incomplete 11",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.1 200 OK\n")),
    ("incomplete 11",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.1 200 OK\r\nA: 1\r")),
    ("incomplete 12",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.1 200 OK\r\nA: 1\r\n")),
    ("slowloris (incomplete)",
     fn () => assertResponseIncomplete (parseResponseString "HTTP/1.0 200 OK\r\n\r")),
    ("slowloris (complete)",
     fn () => assertResponseParsed
                  {minorVersion = 0, status = 200, message = "OK", headers = []}
                  (parseResponseString "HTTP/1.0 200 OK\r\n\r\n")),
    ("invalid http version",
     fn () => assertResponseParseError (fn () => (parseResponseString "HTTP/1. 200 OK\r\n\r\n"))),
    ("invalid http version 2",
     fn () => assertResponseParseError (fn () => (parseResponseString "HTTP/1.2z 200 OK\r\n\r\n"))),
    ("no status code",
     fn () => assertResponseParseError (fn () => (parseResponseString "HTTP/1.1  OK\r\n\r\n")))
]

val headersTest = [
    ("simple",
     fn () => assertHeaderParsed
                  {headers = [("Host", "example.com"), ("Cookie", "")]}
                  (parseHeadersString "Host: example.com\r\nCookie: \r\n\r\n")),
    ("slowloris",
     fn () => assertHeaderParsed
                  {headers = [("Host", "example.com"), ("Cookie", "")]}
                  (parseHeadersString "Host: example.com\r\nCookie: \r\n\r\n")),
    ("partial",
     fn () => assertHeadersIncomplete (parseHeadersString "Host: example.com\r\nCookie: \r\n\r")),
    ("error",
     fn () => assertHeadersParseError (fn () => parseHeadersString "Host: e\127ample.com\r\nCookie: \r\n\r"))
]

fun decodeAtOnce str =
  let
      val size = ref(String.size(str))
      val arr = CharArray.array(!size, #"\000");
      val ret = ref ""
  in
      CharArray.copyVec {di = 0, dst = arr, src = str};
      withDecoder (fn decoder => case decodeChunkedCharArray decoder arr 0 size of
                                     NONE => ret := str
                                   | SOME _ => let
                                       val vec = CharArray.vector arr
                                   in
                                       ret := String.substring(vec, 0, !size)
                                   end);
      !ret
  end

val chunkedDecoderTest = [
    ("1",
     fn () => assertEqualString "hello world" (decodeAtOnce "b\r\nhello world\r\n0\r\n")),
    ("2",
     fn () => assertEqualString "hello world" (decodeAtOnce "6\r\nhello \r\n5\r\nworld\r\n0\r\n")),
    ("3",
     fn () => assertEqualString "hello world" (decodeAtOnce "6;comment=hi\r\nhello \r\n5\r\nworld\r\n0\r\n")),
    ("4",
     fn () => assertEqualString "hello world" (decodeAtOnce "6\r\nhello \r\n5\r\nworld\r\n0\r\na: b\r\nc: d\r\n\r\n")),
    ("5",
     fn () => assertEqualString "hello world" (decodeAtOnce "b\r\nhello world\r\n0\r\n"))
]

fun suite _ = Test.labelTests (requestTests @ responseTests @ headersTest @ chunkedDecoderTest)
end

