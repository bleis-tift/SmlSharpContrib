structure HttpParserTest = struct
open SMLUnit
open HttpParser
open Assert

val assertHeaderParsed =
    assertEqualList(assertEqual2Tuple(assertEqualString
                                     , assertEqualString))
val assertRequestParsed =
    assertEqual4Tuple(assertEqualString,
                      assertEqualString,
                      assertEqualInt,
                      assertHeaderParsed)
val assertResponseParsed =
    assertEqual4Tuple(assertEqualInt, assertEqualInt,
                      assertEqualString,
                      assertHeaderParsed)

fun assertParseError thunk =
  let val  _ = thunk () in fail "exception didn't occured" end
  handle x => assertEqualExceptionName Parse x

fun assertIncomplete thunk =
  let val _ = thunk () in fail "exception didn't occured" end
  handle x => assertEqualExceptionName Incomplete x

val headerArray = (prepareHeaders 100)
val parseRequest' = parseRequest headerArray
val parseResponse' = parseResponse headerArray
val parseHeaders' = parseHeaders headerArray

val requestTests = [
    ("simple",
     fn () => assertRequestParsed
                  ("GET", "/", 0, [])
                  (parseRequest' "GET / HTTP/1.0\r\n\r\n")),
    ("partial",
     fn () => assertIncomplete
                  (fn () => parseRequest' "GET / HTTP/1.0\r\n\r")),
    ("parse headers",
     fn () => assertRequestParsed
                  ("GET", "/hoge", 1, [("Host", "example.com"), ("Cookie", "")])
                  (parseRequest' "GET /hoge HTTP/1.1\r\nHost: example.com\r\nCookie: \r\n\r\n")),
    (* ("multibyte included", *)
    (*  fn () => assertRequestParsed *)
    (*              ("GET", "/hoge", 1, [("Host", "example.com"), ("User-Agent", "\343\201\262\343/1.0")]) *)
    (*              (parseRequest' "GET /hoge HTTP/1.1\r\nHost: example.com\r\nUser-Agent: \343\201\262\343/1.0\r\n\r\n")), *)
    ("parse multiline",
     fn () => assertRequestParsed
                  ("GET", "/", 0, [("foo", ""), ("foo", "b  \tc")])
                  (parseRequest' "GET / HTTP/1.0\r\nfoo: \r\nfoo: b\r\n  \tc\r\n\r\n")),
    ("http header name with trailing space",
     fn () => assertRequestParsed
                  ("GET", "/", 0, [("foo ", "ab")])
                  (parseRequest' "GET / HTTP/1.0\r\nfoo : ab\r\n\r\n")),
    ("incomplete 1",
     fn () => assertIncomplete (fn () => parseRequest' "GET")),
    ("incomplete 2",
     fn () => assertIncomplete (fn () => parseRequest' "GET ")),
    ("incomplete 3",
     fn () => assertIncomplete (fn () => parseRequest' "GET /")),
    ("incomplete 4",
     fn () => assertIncomplete (fn () => parseRequest' "GET / ")),
    ("incomplete 5",
     fn () => assertIncomplete (fn () => parseRequest' "GET / H")),
    ("incomplete 6",
     fn () => assertIncomplete (fn () => parseRequest' "GET / HTTP/1.")),
    ("incomplete 7",
     fn () => assertIncomplete (fn () => parseRequest' "GET / HTTP/1.0")),
    ("incomplete 8",
     fn () => assertIncomplete (fn () => parseRequest' "GET / HTTP/1.0\r")),
    ("slowloris (incomplete)",
     fn () => assertIncomplete (fn () => parseRequest' "GET /hoge HTTP/1.0\r\n\r")),
    ("slowloris (complete)",
     fn () => assertRequestParsed
                  ("GET", "/hoge", 0, [])
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
                  ("GET", "/\160", 0, [("h", "c\162y")])
                  (parseRequest' "GET /\160 HTTP/1.0\r\nh: c\162y\r\n\r\n"))
]

val responseTests = [
    ("simple",
     fn () => assertResponseParsed
                  (0, 200, "OK", [])
                  (parseResponse' "HTTP/1.0 200 OK\r\n\r\n")),
    ("partial",
     fn () => assertIncomplete (fn () =>(parseResponse' "HTTP/1.0 200 OK\r\n\r"))),
    ("parse headers",
     fn () => assertResponseParsed
                  (1, 200, "OK", [("Host", "example.com"), ("Cookie", "")])
                  (parseResponse'"HTTP/1.1 200 OK\r\nHost: example.com\r\nCookie: \r\n\r\n")),
    ("parse multiline",
     fn () => assertResponseParsed
                  (0, 200, "OK", [("foo", ""), ("foo", "b  \tc")])
                  (parseResponse'"HTTP/1.0 200 OK\r\nfoo: \r\nfoo: b\r\n  \tc\r\n\r\n")),
    ("internal server error",
     fn () => assertResponseParsed
                  (0, 500, "Internal Server Error", [])
                  (parseResponse'"HTTP/1.0 500 Internal Server Error\r\n\r\n")),
    ("incomplete 1",
     fn () => assertIncomplete (fn () => (parseResponse' "H"))),
    ("incomplete 2",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1."))),
    ("incomplete 3",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.1"))),
    ("incomplete 4",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.1 "))),
    ("incomplete 5",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.1 2"))),
    ("incomplete 6",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.1 200"))),
    ("incomplete 7",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.1 200 "))),
    ("incomplete 8",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.1 200 O"))),
    ("incomplete 9",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.1 200 OK\r"))),
    ("incomplete 10",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.1 200 OK\r\n"))),
    ("incomplete 11",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.1 200 OK\n"))),
    ("incomplete 11",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.1 200 OK\r\nA: 1\r"))),
    ("incomplete 12",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.1 200 OK\r\nA: 1\r\n"))),
    ("slowloris (incomplete)",
     fn () => assertIncomplete (fn () => (parseResponse' "HTTP/1.0 200 OK\r\n\r"))),
    ("slowloris (complete)",
     fn () => assertResponseParsed
                  (0, 200, "OK", [])
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
                  [("Host", "example.com"), ("Cookie", "")]
                  (parseHeaders' "Host: example.com\r\nCookie: \r\n\r\n")),
    ("slowloris",
     fn () => assertHeaderParsed
                  [("Host", "example.com"), ("Cookie", "")]
                  (parseHeaders' "Host: example.com\r\nCookie: \r\n\r\n")),
    ("partial",
     fn () => assertIncomplete (fn () => parseHeaders' "Host: example.com\r\nCookie: \r\n\r")),
    ("error",
     fn () => assertParseError (fn () => parseHeaders' "Host: e\127ample.com\r\nCookie: \r\n\r"))
]

fun suite _ = Test.labelTests (requestTests @ responseTests @ headersTest)
end

