structure StdTest = struct
  open SMLUnit

  fun id_test () =
    Assert.assertEqualInt 42 (Std.id 42)

  fun suite _ = Test.labelTests [
    ("id test", id_test)
  ]
end
