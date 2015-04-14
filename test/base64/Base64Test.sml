structure Base64Test =
struct
  open SMLUnit
  open Base64

  fun test_wikipedia () =
    (Assert.assertEqualString "QUJDREVGRw==" (encode(stringToWord8Array("ABCDEFG")));
     Assert.assertEqualWord8Array (stringToWord8Array("ABCDEFG")) (decode("QUJDREVGRw==")))

  fun suite _ = Test.labelTests [
        ("test from wikipedia", test_wikipedia)
    ]



end
