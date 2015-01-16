structure StdTest = struct
  open SMLUnit
  open Std

  fun id_test () =
    Assert.assertEqualInt 42 (Std.id 42)

  fun apply_test () =
    let
      fun f x = x * x
      fun g x = x + x
    in
      Assert.assertEqualInt 16 (f o g $ 2)
    end

  fun flip_test () =
    let
      fun f x y = (x * x) + y
    in
      Assert.assertEqualInt 19 ((flip f) 3 4)
    end

  fun curry_test () =
    let
      fun f (x, y) = x + y
    in
      Assert.assertEqualInt 42 ((curry f) 30 12)
    end

  fun uncurry_test () =
    let
      fun f x y = x + y
    in
      Assert.assertEqualInt 42 ((uncurry f) (12, 30))
    end

  fun const_test () =
    let
      val num = const 42
    in
      Assert.assertEqualInt 42 (num 1)
    end

  fun suite _ = Test.labelTests [
    ("id test", id_test),
    ("apply test", apply_test),
    ("flip test", flip_test),
    ("curry test", curry_test),
    ("uncurry test", uncurry_test),
    ("const test", const_test)
  ]
end
