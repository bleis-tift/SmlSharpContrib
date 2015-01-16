structure StackTest = struct
  open Stack
  open SMLUnit

  fun push_peek_test () =
    Assert.assertEqualInt 1 (peekExn(push(1, empty)))

  fun push_pop_test () =
    let
      val s = pop(push(1, empty))
    in
      (Assert.assertSome s;
      Assert.assertTrue (isEmpty (#2 (Option.valOf s))))
    end

  fun suite _ = Test.labelTests [
    ("push_peek test", push_peek_test),
    ("push pop test", push_pop_test)
  ]
end

