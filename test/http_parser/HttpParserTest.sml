structure HttpParserTest = struct
open SMLUnit
open HttpParser
open Assert

val assertEqualHeader =
    assertEqualList(assertEqual2Tuple(assertEqualString,
                                      assertEqualString))

fun assertHeaderParsed expected actual =
  (assertSome actual;
   assertEqualHeader (#headers expected) (#headers (Option.valOf actual)))
    
fun assertRequestParsed expected actual =
  (assertSome actual;
   let
       val a = Option.valOf actual
       val e = expected
   in
       assertEqualString (#method e)       (#method a);
       assertEqualString (#path e)         (#path a);
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
       assertEqualString (#message e) (#message a);
       assertEqualHeader (#headers e) (#headers a)
   end
  )

fun assertParseError thunk =
  let val  _ = thunk () in fail "exception didn't occured" end
  handle x => assertEqualExceptionName Parse x

fun assertIncomplete actual = assertNone actual

val headerArray = (prepareHeaders 100)
val parseRequest' = parseRequest headerArray
val parseResponse' = parseResponse headerArray
val parseHeaders' = parseHeaders headerArray

val requestTests = [
    ("simple",
     fn () => assertRequestParsed
                  {method = "GET", path = "/", minorVersion = 0, headers = []}
                  (parseRequest' "GET / HTTP/1.0\r\n\r\n")),
    ("partial",
     fn () => assertIncomplete (parseRequest' "GET / HTTP/1.0\r\n\r")),
    ("parse headers",
     fn () => assertRequestParsed
                  {method = "GET", path = "/hoge", minorVersion = 1, headers = [("Host", "example.com"), ("Cookie", "")]}
                  (parseRequest' "GET /hoge HTTP/1.1\r\nHost: example.com\r\nCookie: \r\n\r\n")),
    (* ("multibyte included", *)
    (*  fn () => assertRequestParsed *)
    (*              {method = "GET", path = "/hoge", minorVersion = 1, headers = [("Host", "example.com"), ("User-Agent", "\343\201\262\343/1.0")]} *)
    (*              (parseRequest' "GET /hoge HTTP/1.1\r\nHost: example.com\r\nUser-Agent: \343\201\262\343/1.0\r\n\r\n")), *)
    ("parse multiline",
     fn () => assertRequestParsed
                  {method = "GET", path = "/", minorVersion = 0, headers = [("foo", ""), ("foo", "b  \tc")]}
                  (parseRequest' "GET / HTTP/1.0\r\nfoo: \r\nfoo: b\r\n  \tc\r\n\r\n")),
    ("http header name with trailing space",
     fn () => assertRequestParsed
                  {method = "GET", path = "/", minorVersion = 0, headers = [("foo ", "ab")]}
                  (parseRequest' "GET / HTTP/1.0\r\nfoo : ab\r\n\r\n")),
    ("incomplete 1",
     fn () => assertIncomplete (parseRequest' "GET")),
    ("incomplete 2",
     fn () => assertIncomplete (parseRequest' "GET ")),
    ("incomplete 3",
     fn () => assertIncomplete (parseRequest' "GET /")),
    ("incomplete 4",
     fn () => assertIncomplete (parseRequest' "GET / ")),
    ("incomplete 5",
     fn () => assertIncomplete (parseRequest' "GET / H")),
    ("incomplete 6",
     fn () => assertIncomplete (parseRequest' "GET / HTTP/1.")),
    ("incomplete 7",
     fn () => assertIncomplete (parseRequest' "GET / HTTP/1.0")),
    ("incomplete 8",
     fn () => assertIncomplete (parseRequest' "GET / HTTP/1.0\r")),
    ("slowloris (incomplete)",
     fn () => assertIncomplete (parseRequest' "GET /hoge HTTP/1.0\r\n\r")),
    ("slowloris (complete)",
     fn () => assertRequestParsed
                  {method = "GET", path = "/hoge", minorVersion = 0, headers = []}
                  (parseRequest' "GET /hoge HTTP/1.0\r\n\r\n")),
    ("empty header name",
     fn () => assertParseError (fn () => parseRequest' "GET / HTTP/1.0\r\n:a\r\n\r\n")),
    ("header name (space only)",
     fn () => assertParseError (fn () => parseRequest' "GET / HTTP/1.0\r\n :a\r\n\r\n")),
    ("NUL in method",
     fn () => assertParseError (fn () => parseRequest' "G\000T / HTTP/1.0\r\n\r\n")),
    ("tab in method",
     fn () => assertParseError (fn () => parseRequest' "G\tT / HTTP/1.0\r\n\r\n")),
    ("DEL in uri-path",
     fn () => assertParseError (fn () => parseRequest' "GET /\127hello HTTP/1.0\r\n\r\n")),
    ("NUL in header name",
     fn () => assertParseError (fn () => parseRequest' "GET / HTTP/1.0\r\na\000b: c\r\n\r\n")),
    ("NUL in header value",
     fn () => assertParseError (fn () => parseRequest' "GET / HTTP/1.0\r\nab: c\000d\r\n\r\n")),
    ("CTL in header name",
     fn () => assertParseError (fn () => parseRequest' "GET / HTTP/1.0\r\na\027b: c\r\n\r\n")),
    ("CTL in header value",
     fn () => assertParseError (fn () => parseRequest' "GET / HTTP/1.0\r\nab: c\027\r\n\r\n")),
    ("accept MSB chars",
     fn () => assertRequestParsed
                  {method = "GET", path = "/\160", minorVersion = 0, headers = [("h", "c\162y")]}
                  (parseRequest' "GET /\160 HTTP/1.0\r\nh: c\162y\r\n\r\n"))
]

val responseTests = [
    ("simple",
     fn () => assertResponseParsed
                  {minorVersion = 0, status = 200, message = "OK", headers = []}
                  (parseResponse' "HTTP/1.0 200 OK\r\n\r\n")),
    ("partial",
     fn () => assertIncomplete (parseResponse' "HTTP/1.0 200 OK\r\n\r")),
    ("parse headers",
     fn () => assertResponseParsed
                  {minorVersion = 1, status = 200, message = "OK", headers = [("Host", "example.com"), ("Cookie", "")]}
                  (parseResponse'"HTTP/1.1 200 OK\r\nHost: example.com\r\nCookie: \r\n\r\n")),
    ("parse multiline",
     fn () => assertResponseParsed
                  {minorVersion = 0, status = 200, message = "OK", headers = [("foo", ""), ("foo", "b  \tc")]}
                  (parseResponse'"HTTP/1.0 200 OK\r\nfoo: \r\nfoo: b\r\n  \tc\r\n\r\n")),
    ("internal server error",
     fn () => assertResponseParsed
                  {minorVersion = 0, status = 500, message = "Internal Server Error", headers = []}
                  (parseResponse'"HTTP/1.0 500 Internal Server Error\r\n\r\n")),
    ("incomplete 1",
     fn () => assertIncomplete (parseResponse' "H")),
    ("incomplete 2",
     fn () => assertIncomplete (parseResponse' "HTTP/1.")),
    ("incomplete 3",
     fn () => assertIncomplete (parseResponse' "HTTP/1.1")),
    ("incomplete 4",
     fn () => assertIncomplete (parseResponse' "HTTP/1.1 ")),
    ("incomplete 5",
     fn () => assertIncomplete (parseResponse' "HTTP/1.1 2")),
    ("incomplete 6",
     fn () => assertIncomplete (parseResponse' "HTTP/1.1 200")),
    ("incomplete 7",
     fn () => assertIncomplete (parseResponse' "HTTP/1.1 200 ")),
    ("incomplete 8",
     fn () => assertIncomplete (parseResponse' "HTTP/1.1 200 O")),
    ("incomplete 9",
     fn () => assertIncomplete (parseResponse' "HTTP/1.1 200 OK\r")),
    ("incomplete 10",
     fn () => assertIncomplete (parseResponse' "HTTP/1.1 200 OK\r\n")),
    ("incomplete 11",
     fn () => assertIncomplete (parseResponse' "HTTP/1.1 200 OK\n")),
    ("incomplete 11",
     fn () => assertIncomplete (parseResponse' "HTTP/1.1 200 OK\r\nA: 1\r")),
    ("incomplete 12",
     fn () => assertIncomplete (parseResponse' "HTTP/1.1 200 OK\r\nA: 1\r\n")),
    ("slowloris (incomplete)",
     fn () => assertIncomplete (parseResponse' "HTTP/1.0 200 OK\r\n\r")),
    ("slowloris (complete)",
     fn () => assertResponseParsed
                  {minorVersion = 0, status = 200, message = "OK", headers = []}
                  (parseResponse' "HTTP/1.0 200 OK\r\n\r\n")),
    ("invalid http version",
     fn () => assertParseError (fn () => (parseResponse' "HTTP/1. 200 OK\r\n\r\n"))),
    ("invalid http version 2",
     fn () => assertParseError (fn () => (parseResponse' "HTTP/1.2z 200 OK\r\n\r\n"))),
    ("no status code",
     fn () => assertParseError (fn () => (parseResponse' "HTTP/1.1  OK\r\n\r\n")))
]

val headersTest = [
    ("simple",
     fn () => assertHeaderParsed
                  {headers = [("Host", "example.com"), ("Cookie", "")]}
                  (parseHeaders' "Host: example.com\r\nCookie: \r\n\r\n")),
    ("slowloris",
     fn () => assertHeaderParsed
                  {headers = [("Host", "example.com"), ("Cookie", "")]}
                  (parseHeaders' "Host: example.com\r\nCookie: \r\n\r\n")),
    ("partial",
     fn () => assertIncomplete (parseHeaders' "Host: example.com\r\nCookie: \r\n\r")),
    ("error",
     fn () => assertParseError (fn () => parseHeaders' "Host: e\127ample.com\r\nCookie: \r\n\r"))
]

fun decodeAtOnce str =
  let
      val decoder = prepareDecoder()
      val size = ref(String.size(str))
      val arr = CharArray.array(!size, #"\000");
  in
      CharArray.copyVec {di = 0, dst = arr, src = str};
      case decodeChunked decoder arr 0 size of
          NONE => str
        | SOME _ => let
            val vec = CharArray.vector arr
        in
            String.substring(vec, 0, !size)
        end
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

