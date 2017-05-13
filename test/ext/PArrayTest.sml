structure PArrayTest =
struct
  open SMLUnit

  fun updateTest () =
    let
      val t1 = PArray.tabulate (10, fn n => n)
      val t2 = PArray.update (t1, 3, 42)
      val t3 = PArray.update (t2, 1, 250)
      val t4 = PArray.update (t1, 2, 750)
    in
      Assert.assertEqualInt 1 (PArray.sub (t1, 1));
      Assert.assertEqualInt 2 (PArray.sub (t1, 2));
      Assert.assertEqualInt 3 (PArray.sub (t1, 3));
      Assert.assertEqualInt 1 (PArray.sub (t2, 1));
      Assert.assertEqualInt 2 (PArray.sub (t2, 2));
      Assert.assertEqualInt 42 (PArray.sub (t2, 3));
      Assert.assertEqualInt 250 (PArray.sub (t3, 1));
      Assert.assertEqualInt 2 (PArray.sub (t3, 2));
      Assert.assertEqualInt 42 (PArray.sub (t3, 3));
      Assert.assertEqualInt 1 (PArray.sub (t4, 1));
      Assert.assertEqualInt 750 (PArray.sub (t4, 2));
      Assert.assertEqualInt 3 (PArray.sub (t4, 3))
    end

  fun mapiTest () =
    let val t = PArray.mapi op+ (PArray.tabulate (10, fn n => n * n)) in
      Assert.assertEqualInt 0 (PArray.sub (t, 0));
      Assert.assertEqualInt 2 (PArray.sub (t, 1));
      Assert.assertEqualInt 6 (PArray.sub (t, 2));
      Assert.assertEqualInt 12 (PArray.sub (t, 3));
      Assert.assertEqualInt 20 (PArray.sub (t, 4));
      Assert.assertEqualInt 10 (PArray.length t)
    end
      
  fun suite () = Test.labelTests
    [ ("update test", updateTest),
      ("mapi test", mapiTest) ]
end
