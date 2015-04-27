structure Base64Test =
struct
  open SMLUnit
  open Base64

  fun stringToWord8Array str = Word8Array.tabulate(String.size(str), fn i => Byte.charToByte(String.sub(str, i)))

  fun assertEncoded expect str =
    Assert.assertEqualString  expect (encode(stringToWord8Array(str)))
  fun assertDecoded expect str =
    Assert.assertEqualWord8Array (stringToWord8Array expect) (decode str)
  fun assertBoth encoded decoded =
    (assertEncoded encoded decoded;
     assertDecoded decoded encoded)

  fun suite _ = Test.labelTests [
        ("test from wikipedia",    fn () => assertBoth "QUJDREVGRw==" "ABCDEFG"),
        ("Two padding characters", fn () => assertBoth "YQ==" "a"),
        ("One padding character",  fn () => assertBoth "YWE=" "aa"),
        ("No padding characters",  fn () => assertBoth "YWFh" "aaa"),
        ("U+0000",                 fn () => assertBoth "Zm9vAA=="  "foo\000"),
        ("U+0000",                 fn () => assertBoth "Zm9vAAA="  "foo\000\000"),
        ("U+0000 + a character",   fn () => assertBoth "AGE="  "\000a"),
        ("foo bar baz",            fn () => assertBoth "Zm9vIGJhciBiYXo=" "foo bar baz"),
        ("foo bar",                fn () => assertBoth "Zm9vIGJhcg==" "foo bar"),
        ("foo",                    fn () => assertBoth "Zm9v" "foo")
            
    ]



end
