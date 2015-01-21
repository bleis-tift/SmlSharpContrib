structure EitherTest = struct
  open SMLUnit
  open Either
  open Std

  fun assertEqualEither x y =
    Assert.assertEqual (equal (op=) (op=)) (fn _ => "") x y

  fun inc n =
    n + 1

  fun mapLeftTest () =
    (assertEqualEither (Left 1) $ mapLeft inc $ Left 0;
     assertEqualEither (Right 0) $ mapLeft inc $ Right 0)

  fun mapRightTest () =
    (assertEqualEither (Left 0) $ mapRight inc $ Left 0;
     assertEqualEither (Right 1) $ mapRight inc $ Right 0)

  fun isLeftTest () =
    (Assert.assertEqualBool true $ isLeft (Left 42);
     Assert.assertEqualBool false $ isLeft (Right 42))

  fun isRightTest () =
    (Assert.assertEqualBool true $ isRight (Right 42);
     Assert.assertEqualBool false $ isRight (Left 42))

  fun suite _ = Test.labelTests [
    ("mapLeftTest", mapLeftTest),
    ("mapRightTest", mapRightTest),
    ("isLeftTest", isLeftTest),
    ("isRightTest", isRightTest)
  ]
end
