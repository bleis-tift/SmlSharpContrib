structure JsonDecoderTest = struct
  open SMLUnit
  open JsonValue
  open JsonDecoder

  fun assertDecode expected actual =
    (Assert.assertSome actual;
    Assert.assertTrue (equal ((Option.valOf actual), expected)))

  fun empty_object_test () =
    let
      val expected = JsonObject []
      val actual = decode "{}"
    in
      assertDecode expected actual
    end

  fun some_object_test () =
    let
      val expected = JsonObject [
        ("value1", JsonNumber 1.0),
        ("value2", JsonBool true)
      ]
      val actual = decode "{ \"value1\" : 1.0, \"value2\" : true }"
    in
      assertDecode expected actual
    end

  fun nested_object_test () =
    let
      val expected = JsonObject [
        ("a", JsonObject [ ("b", JsonNull) ])
      ]
      val actual = decode "{ \"a\" : { \"b\" : null } }"
    in
      assertDecode expected actual
    end

  fun empty_array_test () =
    let
      val expected = JsonArray []
      val actual = decode "[]"
    in
      assertDecode expected actual
    end

  fun bool_test () =
    let
      val expected = JsonArray [
        JsonBool true,
        JsonBool false
      ]
      val actual = decode "[true, false]"
    in
      assertDecode expected actual
    end

  fun int_test () =
    let
      val expected = JsonArray [
        JsonNumber 0.0,
        JsonNumber 1.0,
        JsonNumber 10.0,
        JsonNumber ~1.0,
        JsonNumber ~10.0
      ]
      val actual = decode "[ 0, 1, 10, -1, -10 ]"
    in
      assertDecode expected actual
    end

  fun real_test () =
    let
      val expected = JsonArray [
        JsonNumber 0.0,
        JsonNumber 1.0,
        JsonNumber 1.01,
        JsonNumber ~0.2
      ]
      val actual = decode "[ 0.0, 1., 1.01, -0.2 ]"
    in
      assertDecode expected actual
    end

  fun exp_num_test () =
    let
      val expected = JsonArray [
        JsonNumber 0.0,
        JsonNumber 1.0,
        JsonNumber 1.0,
        JsonNumber 10.0,
        JsonNumber 0.1,
        JsonNumber 0.0,
        JsonNumber 1.0,
        JsonNumber 1.0,
        JsonNumber 10.0,
        JsonNumber 0.1
      ]
      val actual = decode "[ 0E, 1E+, 1E-, 1E+1, 1E-1, 0e, 1e+, 1e-, 1e+1, 1e-1 ]"
    in
      assertDecode expected actual
    end

  fun frac_exp_test () =
    let
      val expected = JsonArray [
        JsonNumber 0.0,
        JsonNumber 0.0,
        JsonNumber 1.0,
        JsonNumber 0.2
      ]
      val actual = decode "[ 0.0E, 0.0e, 0.1E+1, 2.0e-1 ]"
    in
      assertDecode expected actual
    end

  fun string_test () =
    let
      val expected = JsonArray [
        JsonString "",
        JsonString "a"
      ]
      val actual = decode "[ \"\", \"a\" ]"
    in
      assertDecode expected actual
    end

  fun nested_array_test () =
    let
      val expected = JsonArray [
        JsonArray [],
        JsonArray [ JsonString "a" ]
      ]
      val actual = decode "[ [], [\"a\"] ]"
    in
      assertDecode expected actual
    end

  fun suite _ = Test.labelTests [
    ("empty object test", empty_object_test),
    ("some object test", some_object_test),
    ("nested object test", nested_object_test),
    ("empty arrray test", empty_array_test),
    ("bool test", bool_test),
    ("int test", int_test),
    ("real test", real_test),
    ("exp num test", exp_num_test),
    ("frac exp test", frac_exp_test),
    ("string test", string_test),
    ("nested array test", nested_array_test)
  ]
end

