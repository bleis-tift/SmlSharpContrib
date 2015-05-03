structure ShowTest =
struct
open SMLUnit
open Show
val assertEqualString =  Assert.assertEqualString

fun suite _ = Test.labelTests [
      ("showUnit:",
       fn () => assertEqualString "()" (showUnit ())),
      ("showInt: 0",
       fn () => assertEqualString "0" (showInt 0)),
      ("showInt: 123",
       fn () => assertEqualString "123" (showInt 123)),
      ("showInt: 0x7B",
       fn () => assertEqualString "123" (showInt 0x7B)),
      ("showInt: ~123",
       fn () => assertEqualString "~123" (showInt ~123)),
      ("showInt: ~0x7B",
       fn () => assertEqualString "~123" (showInt ~0x7B)),
      ("showWord: 0",
       fn () => assertEqualString "0wx0" (showWord 0w0)),
      ("showWord: 123",
       fn () => assertEqualString "0wx7B" (showWord 0w123)),
      ("showWord: 0x7B",
       fn () => assertEqualString "0wx7B" (showWord 0wx7B)),
      ("showWord8: 0",
       fn () => assertEqualString "0wx0" (showWord8 0w0)),
      ("showWord8: 123",
       fn () => assertEqualString "0wx7B" (showWord8 0w123)),
      ("showWord8: 0x7B",
       fn () => assertEqualString "0wx7B" (showWord8 0wx7B)),
      ("showWord32: 0",
       fn () => assertEqualString "0wx0" (showWord32 0w0)),
      ("showWord32: 123",
       fn () => assertEqualString "0wx7B" (showWord32 0w123)),
      ("showWord32: 0x7B",
       fn () => assertEqualString "0wx7B" (showWord32 0wx7B)),
      ("showReal: 0.0",
       fn () => assertEqualString "0.0" (showReal 0.0)),
      ("showReal: ~0.0",
       fn () => assertEqualString "~0.0" (showReal ~0.0)),
      ("showReal: 1.0",
       fn () => assertEqualString "1.0" (showReal 1.0)),
      ("showReal: ~1.0",
       fn () => assertEqualString "~1.0" (showReal ~1.0)),
      ("showReal: 0.01",
       fn () => assertEqualString "0.01" (showReal 0.01)),
      ("showReal: ~0.01",
       fn () => assertEqualString "~0.01" (showReal ~0.01)),
      ("showReal: 0.010",
       fn () => assertEqualString "0.01" (showReal 0.010)),
      ("showChar: #\"a\"",
       fn () => assertEqualString "#\"a\"" (showChar #"a")),
      ("showChar: #\"\\\"",
       fn () => assertEqualString "#\"\\\\\"" (showChar #"\\")),
      ("showChar: #\"\"\"",
       fn () => assertEqualString "#\"\\\"\"" (showChar #"\"")),
      ("showChar: #\"\097\"",
       fn () => assertEqualString "#\"a\"" (showChar #"\097")),
      ("showString: \"a\"",
       fn () => assertEqualString "\"a\"" (showString "a")),
      ("showString: \"\\\"\"",
       fn () => assertEqualString "\"\\\"\"" (showString "\"")),
      ("showBool: true",
       fn () => assertEqualString "true" (showBool true)),
      ("showBool: false",
       fn () => assertEqualString "false" (showBool false)),
      ("showException: Fail",
       fn () => assertEqualString "Fail" (showException (Fail ""))),
      ("showRef: ref ()",
       fn () => assertEqualString "ref ()" (showRef showUnit (ref ()))),
      ("showOption: NONE",
       fn () => assertEqualString "NONE" (showOption showUnit NONE)),
      ("showOption: SOME()",
       fn () => assertEqualString "SOME ()" (showOption showUnit (SOME()))),
      ("showOrder: GREATER",
       fn () => assertEqualString "GREATER" (showOrder GREATER)),
      ("showOrder: EQUAL",
       fn () => assertEqualString "EQUAL" (showOrder EQUAL)),
      ("showOrder: LESS",
       fn () => assertEqualString "LESS" (showOrder LESS)),
      ("show2Tuple: ((), 1)",
       fn () => assertEqualString "((), 1)"
                                  (show2Tuple (showUnit, showInt)
                                              ((), 1))),
      ("show3Tuple: ((), 1, 0wx2)",
       fn () => assertEqualString "((), 1, 0wx2)"
                                  (show3Tuple (showUnit, showInt, showWord)
                                              ((), 1, 0wx2))),
      ("show4Tuple: ((), 1, 0wx2, 3.0)",
       fn () => assertEqualString "((), 1, 0wx2, 3.0)"
                                  (show4Tuple (showUnit, showInt, showWord, showReal)
                                              ((), 1, 0wx2, 3.0))),
      ("show5Tuple: ((), 1, 0wx2, 3.0, #\"4\")",
       fn () => assertEqualString "((), 1, 0wx2, 3.0, #\"4\")"
                                  (show5Tuple (showUnit, showInt, showWord, showReal, showChar)
                                              ((), 1, 0wx2, 3.0, #"4"))),
      ("show6Tuple: ((), 1, 0wx2, 3.0, #\"4\", \"5\")",
       fn () => assertEqualString "((), 1, 0wx2, 3.0, #\"4\", \"5\")"
                                  (show6Tuple (showUnit, showInt, showWord, showReal, showChar, showString)
                                              ((), 1, 0wx2, 3.0, #"4", "5"))),
      ("show7Tuple: ((), 1, 0wx2, 3.0, #\"4\", \"5\", true)",
       fn () => assertEqualString "((), 1, 0wx2, 3.0, #\"4\", \"5\", true)"
                                  (show7Tuple (showUnit, showInt, showWord, showReal, showChar, showString, showBool)
                                              ((), 1, 0wx2, 3.0, #"4", "5", true))),
      ("show8Tuple: ((), 1, 0wx2, 3.0, #\"4\", \"5\", true, Fail)",
       fn () => assertEqualString "((), 1, 0wx2, 3.0, #\"4\", \"5\", true, Fail)"
                                  (show8Tuple (showUnit, showInt, showWord, showReal, showChar, showString, showBool, showException)
                                              ((), 1, 0wx2, 3.0, #"4", "5", true, Fail ""))),
      ("show9Tuple: ((), 1, 0wx2, 3.0, #\"4\", \"5\", true, Fail, GREATER)",
       fn () => assertEqualString "((), 1, 0wx2, 3.0, #\"4\", \"5\", true, Fail, GREATER)"
                                  (show9Tuple (showUnit, showInt, showWord, showReal, showChar, showString, showBool, showException, showOrder)
                                              ((), 1, 0wx2, 3.0, #"4", "5", true, Fail "", GREATER))),
      ("show10Tuple: ((), 1, 0wx2, 3.0, #\"4\", \"5\", true, Fail, GREATER, ref 9)",
       fn () => assertEqualString "((), 1, 0wx2, 3.0, #\"4\", \"5\", true, Fail, GREATER, ref 9)"
                                  (show10Tuple (showUnit, showInt, showWord, showReal, showChar, showString, showBool, showException, showOrder, showRef showInt)
                                               ((), 1, 0wx2, 3.0, #"4", "5", true, Fail "", GREATER, ref 9))),
      ("showVector: #<Vector: \"\", \"\", \"\">",
       fn () => assertEqualString "#<Vector: \"\", \"\", \"\">" (showVector showString (Vector.tabulate(3, fn _ => "")))),
      ("showWord8Array: #<Array: 0wx1, 0wx2, 0wx3>",
       fn () => assertEqualString "#<Array: 0wx1, 0wx2, 0wx3>" (showWord8Array (Array.tabulate(3, fn i => Word8.fromInt(i + 1))))),
      ("showRealList: [0.1, 0.2, 0.3]",
       fn () => assertEqualString "[0.1, 0.2, 0.3]" (showRealList [0.1 ,0.2, 0.3])),
      ("ancher",
       fn () => ())
  ]
end
