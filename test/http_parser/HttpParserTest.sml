structure HttpParserTest = struct
open SMLUnit
open HttpParser
open Assert

val assertParsed =
    assertEqual4Tuple(assertEqualString,
                      assertEqualString,
                      assertEqualInt,
                      assertEqualList(assertEqual2Tuple(assertEqualString
                                                       , assertEqualString)))

fun assertParseError thunk =
  let val  _ = thunk () in fail "exception didn't occured" end
  handle x => assertEqualExceptionName Parse x

fun assertRequestIncomplete thunk =
  let val _ = thunk () in fail "exception didn't occured" end
  handle x => assertEqualExceptionName RequestIncomplete x

val parseRequest' = parseRequest (prepareHeaders 100)

fun suite _ = Test.labelTests [
      ("simple",
       fn () => assertParsed
                    ("GET", "/", 0, [])
                    (parseRequest' "GET / HTTP/1.0\r\n\r\n")),
      ("partial",
       fn () => assertRequestIncomplete
                    (fn () => parseRequest' "GET / HTTP/1.0\r\n\r")),
      ("parse headers",
       fn () => assertParsed
                    ("GET", "/hoge", 1, [("Host", "example.com"), ("Cookie", "")])
                    (parseRequest' "GET /hoge HTTP/1.1\r\nHost: example.com\r\nCookie: \r\n\r\n")),
      (* ("multibyte included", *)
      (*  fn () => assertParsed *)
      (*              ("GET", "/hoge", 1, [("Host", "example.com"), ("User-Agent", "\343\201\262\343/1.0")]) *)
      (*              (parseRequest' "GET /hoge HTTP/1.1\r\nHost: example.com\r\nUser-Agent: \343\201\262\343/1.0\r\n\r\n")), *)
      ("parse multiline",
       fn () => assertParsed
                    ("GET", "/", 0, [("foo", ""), ("foo", "b  \tc")])
                    (parseRequest' "GET / HTTP/1.0\r\nfoo: \r\nfoo: b\r\n  \tc\r\n\r\n")),
      ("http header name with trailing space",
       fn () => assertParsed
                   ("GET", "/", 0, [("foo ", "ab")])
                   (parseRequest' "GET / HTTP/1.0\r\nfoo : ab\r\n\r\n")),
      ("incomplete 1",
       fn () => assertRequestIncomplete (fn () => parseRequest' "GET")),
      ("incomplete 2",
       fn () => assertRequestIncomplete (fn () => parseRequest' "GET ")),
      ("incomplete 3",
       fn () => assertRequestIncomplete (fn () => parseRequest' "GET /")),
      ("incomplete 4",
       fn () => assertRequestIncomplete (fn () => parseRequest' "GET / ")),
      ("incomplete 5",
       fn () => assertRequestIncomplete (fn () => parseRequest' "GET / H")),
      ("incomplete 6",
       fn () => assertRequestIncomplete (fn () => parseRequest' "GET / HTTP/1.")),
      ("incomplete 7",
       fn () => assertRequestIncomplete (fn () => parseRequest' "GET / HTTP/1.0")),
      ("incomplete 8",
       fn () => assertRequestIncomplete (fn () => parseRequest' "GET / HTTP/1.0\r")),
      ("slowloris (incomplete)",
       fn () => assertRequestIncomplete (fn () => parseRequest' "GET /hoge HTTP/1.0\r\n\r")),
      ("slowloris (complete)",
       fn () => assertParsed
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
       (fn () => assertParsed
                    ("GET", "/\160", 0, [("h", "c\162y")])
                    (parseRequest' "GET /\160 HTTP/1.0\r\nh: c\162y\r\n\r\n")))
  ]
end

