structure SuspTest = struct
  open SMLUnit

  exception Exit

  fun succ n = n + 1

  fun constructorsTest () =
      let
          val v = 42
          val p1 = Susp.fromVal v
          val p2 = Susp.delay (fn () => v)
          val p3 = Susp.lazy (fn () => p1)
          val p4 = Susp.lazy (fn () => p2)
          val p5 = Susp.lazy (fn () => p3)
      in
          Assert.assertEqualInt v (Susp.force p1);
          Assert.assertEqualInt v (Susp.force p2);
          Assert.assertEqualInt v (Susp.force p3);
          Assert.assertEqualInt v (Susp.force p4);
          Assert.assertEqualInt v (Susp.force p5)
      end

  fun assertRaiseException ex thunk =
      (thunk (); Assert.fail "no exception")
      handle exn =>
             Assert.assertEqualExceptionName ex exn

  fun exnTest () =
      let
          val p = Susp.delay (fn () => raise Exit)
      in
          assertRaiseException Exit (fn () => Susp.force p);
          assertRaiseException Exit (fn () => Susp.force p)
      end

  fun fromVal_isVal_Test () =
      Assert.assertTrue (Susp.isVal (Susp.fromVal 42))

  fun unevaluated_isVal_Test () =
      let
          val p1 = Susp.delay (fn () => 42)
          val p2 = Susp.lazy (fn () => Susp.fromVal 42)
      in
          Assert.assertFalse (Susp.isVal p1);
          Assert.assertFalse (Susp.isVal p2)
      end

  fun forced_isVal_Test () =
      let
          val p1 = Susp.delay (fn () => 42)
          val p2 = Susp.lazy (fn () => Susp.fromVal 42)
          val _ = Susp.force p1
          val _ = Susp.force p2
      in
          Assert.assertTrue (Susp.isVal p1);
          Assert.assertTrue (Susp.isVal p2)
      end

  fun ignoreErrors f =
      f () handle _ => ()

  fun exn_isVal_Test () =
      let
          val p = Susp.delay (fn () => raise Exit)
      in
          Assert.assertFalse (Susp.isVal p);
          ignoreErrors (fn () => Susp.force p);
          Assert.assertFalse (Susp.isVal p)
      end

  fun isEvaluatedTest () =
      let
          val pv = Susp.fromVal 42
          val pf = Susp.delay (fn () => 42)
          val pe = Susp.delay (fn () => raise Exit)
          val _ = Assert.assertTrue (Susp.isEvaluated pv)
          val _ = Assert.assertFalse (Susp.isEvaluated pf)
          val _ = Assert.assertFalse (Susp.isEvaluated pe)
          val _ = Susp.force pv
          val _ = Susp.force pf
          val _ = ignoreErrors (fn () => Susp.force pe)
      in
          Assert.assertTrue (Susp.isEvaluated pv);
          Assert.assertTrue (Susp.isEvaluated pf);
          Assert.assertTrue (Susp.isEvaluated pe)
      end

  fun isExnTest () =
      let
          val v = 42
          val pv = Susp.fromVal v
          val pf = Susp.delay (fn () => v)
          val pe = Susp.delay (fn () => raise Exit)
      in
          Assert.assertFalse (Susp.isExn pv);
          Assert.assertFalse (Susp.isExn pf);
          Assert.assertFalse (Susp.isExn pe);
          ignore (Susp.force pv);
          ignore (Susp.force pf);
          ignoreErrors (fn () => Susp.force pe);
          Assert.assertFalse (Susp.isExn pv);
          Assert.assertFalse (Susp.isExn pf);
          Assert.assertTrue (Susp.isExn pe)
      end

  fun peekTest () =
      let
          val v = 42
          val pv = Susp.fromVal v
          val pf = Susp.delay (fn () => v)
          val pe = Susp.delay (fn () => raise Exit)
      in
          Assert.assertEqualIntOption (SOME 42) (Susp.peek pv);
          Assert.assertEqualIntOption NONE (Susp.peek pf);
          Assert.assertEqualIntOption NONE (Susp.peek pe);
          ignore (Susp.force pv);
          ignore (Susp.force pf);
          ignoreErrors (fn () => ignore (Susp.force pe));
          Assert.assertEqualIntOption (SOME 42) (Susp.peek pv);
          Assert.assertEqualIntOption (SOME 42) (Susp.peek pf);
          Assert.assertEqualIntOption NONE (Susp.peek pe)
      end

  fun mapTest () =
      let
          fun iterate n f x =
              if n <= 0 then
                  x
              else
                  iterate (n - 1) f (f x)
          val n = 4
          val p = Susp.fromVal 0
      in
          Assert.assertEqualInt n (Susp.force (iterate n (Susp.map succ) p))
      end

  fun bindTest () =
      let
          val x = 1
          val y = 2
          val p =
              Susp.bind (Susp.fromVal x) (fn i =>
              Susp.bind (Susp.delay (fn () => y)) (fn j =>
              Susp.fromVal (i + j)))
      in
          Assert.assertEqualInt (x + y) (Susp.force p)
      end

  (*-------------------- Tests from SRFI-45 --------------------*)
  structure Stream = struct
    datatype 'a node = Nil | Cons of 'a * 'a t
    withtype 'a t = 'a node Susp.t

    exception Empty

    fun hd xs = case Susp.force xs of
                    Nil => raise Empty
                 | Cons (x, _) => x

    fun tl xs = case Susp.force xs of
                    Nil => raise Empty
                 | Cons (_, xs') => xs'

    fun drop 0 xs = xs
      | drop n xs = drop (n - 1) (tl xs)
  end

  fun memoizationTest1 () =
      let
          val buf = ref ""
          val s = Susp.delay (fn () => (buf := !buf ^ "hello"; 1))
          val _ = Susp.force s
          val _ = Susp.force s
      in
          Assert.assertEqualString "hello" (!buf)
      end

  fun memoizationTest2 ctx =
      let
          val buf = ref ""
          val s = Susp.delay (fn () => (buf := !buf ^ "bonjour"; 2))
          val _ = Susp.force s + Susp.force s
      in
          Assert.assertEqualString "bonjour" (!buf)
      end

  fun memoizationTest3 () =
      let
          val buf = ref ""
          val r = Susp.delay (fn () => (buf := !buf ^ "hi"; 1))
          val s = Susp.lazy (fn () => r)
          val t = Susp.lazy (fn () => s)
          val _ = Susp.force t
          val _ = Susp.force r
      in
          Assert.assertEqualString "hi" (!buf)
      end

  fun memoizationTest4 () =
      let
          val buf = ref ""
          fun ones () = Susp.delay (fn () => (
                                        buf := !buf ^ "ho";
                                        Stream.Cons (1, ones ())))
          val s = ones ()
          val _ = Stream.hd (Stream.drop 4 s)
          val _ = Stream.hd (Stream.drop 4 s)
      in
          Assert.assertEqualString "hohohohoho" (!buf)
      end

  fun reentrancyTest1 () =
      let
          val count = ref 0
          val x = ref 0
          val p = ref (Susp.fromVal 0)
          val p' = Susp.delay (fn () => (
            count := !count + 1;
            if !count > !x then
                !count
            else
                Susp.force (!p)))
      in
          p := p';
          x := 5;
          Assert.assertEqualInt 6 (Susp.force p');
          x := 10;
          Assert.assertEqualInt 6 (Susp.force p')
      end

  fun reentrancyTest2 () =
      let
          val f = ref (Susp.fromVal "")
          val f_ =
              let val first = ref true in
                  Susp.delay (fn () =>
                    if !first then (
                        first := false;
                        Susp.force (!f)
                    ) else
                        "second")
              end
      in
          f := f_;
          Assert.assertEqualString "second" (Susp.force (!f))
      end

  fun reentrancyTest3 () =
      let
          val (getCount, p) =
              let
                  val count = ref 5
                  fun getCount () = !count
                  val p = ref (Susp.fromVal 0)
                  val p' =
                      Susp.delay
                          (fn () =>
                              if !count <= 0 then
                                  !count
                              else
                                  let
                                      val _ = count := !count - 1
                                      val _ = Susp.force (!p)
                                  in
                                      count := !count + 2;
                                      !count
                                  end)
              in
                  p := p';
                  (getCount, !p)
              end
      in
          Assert.assertEqualInt 5 (getCount ());
          Assert.assertEqualInt 0 (Susp.force p);
          Assert.assertEqualInt 10 (getCount ())
      end

  fun suite () = Test.labelTests
      [ ("constructors test", constructorsTest)
      , ("test exn", exnTest)
      , ("test fromVal isVal", fromVal_isVal_Test)
      , ("test unevaluated isVal", unevaluated_isVal_Test)
      , ("test force'd isVal", forced_isVal_Test)
      , ("test exn not isVal", exn_isVal_Test)
      , ("isEvaluated test", isEvaluatedTest)
      , ("isExn test", isExnTest)
      , ("peek test", peekTest)
      , ("map test", mapTest)
      , ("bind test", bindTest)
      , ("Memoization test 1", memoizationTest1)
      , ("Memoization test 2", memoizationTest2)
      , ("Memoization test 3", memoizationTest3)
      , ("Memoization test 4", memoizationTest4)
      , ("Reentrancy test 1", reentrancyTest1)
      , ("Reentrancy test 2", reentrancyTest2)
      , ("Reentrancy test 3", reentrancyTest3)
      ]
end
