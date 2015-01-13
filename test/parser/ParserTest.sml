structure ParserTest = struct
  open SMLUnit
  open Parser

  fun testParseString input p expected =
    let
      val actual = parse p input
    in
      let
        val (r, i) = Option.valOf actual
      in
        Assert.assertEqualString expected r
      end
    end

  fun any_test () =
    let
      val input = "a"
    in
      testParseString input (any input) "a"
    end

  fun pstring_test () =
    let
      val input = "test"
    in
      testParseString input (pstring "test" input) "test"
    end

  fun orElse_test () =
    let
      val input = "abcde"
      val p = orElse (pstring "not hit" input) (pstring "abc" input)
    in
      testParseString input p "abc"
    end

  fun testParseStringList input p expected =
    let
      val actual = parse p input
    in
      let
        val (r, i) = Option.valOf actual
      in
        Assert.assertEqualStringList expected r
      end
    end

  fun many_but_input_is_empty () =
    let
      val input = ""
    in
      testParseStringList input (many (any input)) []
    end

  fun many_test () =
    let
      val input = "abcde"
    in
      testParseStringList input (many (any input)) ["a","b","c","d","e"]
    end

  fun many1_fail_test () =
    let
      val input = "bababab"
      val p = many1 (pstring "ab" input)
    in
      Assert.assertNone (parse p input)
    end

  fun many1_test () =
    let
      val input = "ababab"
      val p = many1 (pstring "ab" input)
    in
      testParseStringList input p ["ab","ab","ab"]
    end

  fun testParseString2Tuple input p expected =
    let
      val actual = parse p input
    in
      let
        val (r, i) = Option.valOf actual
      in
        Assert.assertEqual2Tuple
          (Assert.assertEqualString, Assert.assertEqualString)
          expected
          r
      end
    end

  fun andThen_test () =
    let
      val input = "ab"
      val p = andThen (pstring "a" input) (pstring "b" input)
    in
      testParseString2Tuple input p ("a", "b")
    end

  fun suite _ = Test.labelTests [
    ("any test", any_test),
    ("pstring test", pstring_test),
    ("orElse test", orElse_test),
    ("many but input is empty", many_but_input_is_empty),
    ("many test", many_test),
    ("many1 fail test", many1_fail_test),
    ("many1 test", many1_test),
    ("andThen test", andThen_test)
  ]
end

