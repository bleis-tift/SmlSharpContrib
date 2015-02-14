structure JsonEncoderTest = struct
  open SMLUnit
  open JsonValue
  open JsonEncoder

  fun assertEncode expected actual =
    (Assert.assertSome actual;
    Assert.assertEqualString expected (Option.valOf actual))

  fun empty_object_test () =
    let
      val expected = "{}"
      val actual = encode Args.minify (JsonObject [])
    in
      assertEncode expected actual
    end

  fun some_object_test () =
    let
      val input = JsonObject [
        ("value1", JsonNumber 1.0),
        ("value2", JsonBool true)
      ]
      val expected = "{\"value1\":1.0,\"value2\":true}"
      val actual = encode Args.minify input
    in
      assertEncode expected actual
    end

  fun nested_object_test () =
    let
      val input = JsonObject [
        ("a", JsonObject [ ("b", JsonNull) ])
      ]
      val expected = "{\"a\":{\"b\":null}}"
      val actual = encode Args.minify input
    in
      assertEncode expected actual
    end

  fun nested_object_space_test () =
    let
      val input = JsonObject [
        ("a", JsonObject [ ("b", JsonNull) ])
      ]
      val expected =
        "{\n  \"a\" : {\n    \"b\" : null\n  }\n}"
      val actual = encode Args.twoSpaceLf input
    in
      assertEncode expected actual
    end

  fun empty_array_test () =
    let
      val input = JsonArray []
      val expected = "[]"
      val actual = encode Args.minify input
    in
      assertEncode expected actual
    end

  fun bool_test () =
    let
      val input = JsonArray [
        JsonBool true,
        JsonBool false
      ]
      val expected = "[true,false]"
      val actual = encode Args.minify input
    in
      assertEncode expected actual
    end

  fun real_test () =
    let
      val input = JsonArray [
        JsonNumber 0.0,
        JsonNumber 1.0,
        JsonNumber 1.01,
        JsonNumber ~0.2
      ]
      val expected = "[0.0,1.0,1.01,-0.2]"
      val actual = encode Args.minify input
    in
      assertEncode expected actual
    end

  fun string_test () =
    let
      val input = JsonArray [
        JsonString "",
        JsonString "a"
      ]
      val expected = "[\"\",\"a\"]"
      val actual = encode Args.minify input
    in
      assertEncode expected actual
    end

  fun nested_array_test () =
    let
      val input = JsonArray [
        JsonArray [],
        JsonArray [ JsonString "a" ]
      ]
      val expected = "[[],[\"a\"]]"
      val actual = encode Args.minify input
    in
      assertEncode expected actual
    end

  fun nested_space_test () =
    let
      val input = JsonArray [
        JsonArray [],
        JsonArray [ JsonString "a", JsonString "b" ]
      ]
      val expected = "[\n  [],\n  [\n    \"a\",\n    \"b\"\n  ]\n]"
      val actual = encode Args.twoSpaceLf input
    in
      assertEncode expected actual
    end

  fun suite _ = Test.labelTests [
    ("empty object test", empty_object_test),
    ("some object test", some_object_test),
    ("nested object test", nested_object_test),
    ("nested object_space test", nested_object_space_test),
    ("empty arrray test", empty_array_test),
    ("bool test", bool_test),
    ("number test", real_test),
    ("string test", string_test),
    ("nested array test", nested_array_test),
    ("nested space test", nested_space_test)
  ]
end

