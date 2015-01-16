structure QueueTest = struct
  open Queue
  open SMLUnit

  fun enqueue_top_test () =
    Assert.assertEqualInt 1 (peekExn(enqueue(empty, 1)))

  fun enqueue_dequeue_test () =
    let
      val q = dequeue(enqueue(empty, 1))
    in
      (Assert.assertSome q;
      Assert.assertTrue (isEmpty (#2 (Option.valOf q))))
    end

  fun size_test () =
    Assert.assertEqualInt 4
      (size (enqueue(enqueue(enqueue(enqueue(empty, 1), 2), 3) ,4)))

  fun map_fold_test () =
    let
      val input = [0,1,2]
      val q =
        List.foldl (fn (x, a) => enqueue(a, x)) empty input
      val mapped = map (fn x => x + 1) q
      val actual = List.rev (fold (op ::) [] mapped)
    in
      Assert.assertEqualIntList [1,2,3] actual
    end

  fun peek_test () =
    Assert.assertNone (peek empty)

  fun suite _ = Test.labelTests [
    ("enqueue top test", enqueue_top_test),
    ("enqueue dequeue test", enqueue_dequeue_test),
    ("size_test", size_test),
    ("map_fold_test", map_fold_test),
    ("peek test", peek_test)
  ]
end

