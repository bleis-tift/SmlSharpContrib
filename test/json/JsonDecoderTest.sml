structure JsonDecoderTest = struct
  open SMLUnit
  open JsonValue
  open JsonDecoder

  fun empty_object_test () =
    let
      val actual = decode "{}"
    in
      Assert.assertSome actual
    end

  fun some_object_test () =
    let
      val actual = decode "{ \"value1\" : 1.0, \"value2\" : true }"
    in
      Assert.assertSome actual
    end

  fun nested_object_test () =
    let
      val actual = decode "{ \"a\" : { \"b\" : null } }"
    in
      Assert.assertSome actual
    end

  fun empty_array_test () =
    let
      val actual = decode "[]"
    in
      Assert.assertSome actual
    end

  fun bool_test () =
    let
      val actual = decode "[true, false]"
    in
      Assert.assertSome actual
    end

  fun int_test () =
    let
      val actual = decode "[ 0, 1, 10, -1, -10 ]"
    in
      Assert.assertSome actual
    end

  fun real_test () =
    let
      val actual = decode "[ 0.0, 1., 1.01, -0.2 ]"
    in
      Assert.assertSome actual
    end

  fun exp_num_test () =
    let
      val actual = decode "[ 0E, 1E+, 1E-, 1E+1, 1E-1, 0e, 1e+, 1e-, 1e+1, 1e-1 ]"
    in
      Assert.assertSome actual
    end

  fun frac_exp_test () =
    let
      val actual = decode "[ 0.0E, 0.0e, 0.1E+1, 2.0e-1 ]"
    in
      Assert.assertSome actual
    end

  fun string_test () =
    let
      val actual = decode "[ \"\", \"a\" ]"
    in
      Assert.assertSome actual
    end

  fun nested_array_test () =
    let
      val actual = decode "[ [], [\"a\"] ]"
    in
      Assert.assertSome actual
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

