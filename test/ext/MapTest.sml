structure MapTest =
struct
  open SMLUnit
  
  structure OrderedInt =
  struct
    type t = int
    fun compare (n, m) =
      if n < m then LESS
      else if n = m then EQUAL
      else GREATER
  end

  structure IntMap = Map (OrderedInt)

  fun imapFromList l =
    foldl (fn ((n, d), s) => IntMap.add n d s) IntMap.empty l
  fun assertEqualIntMap cmp s1 s2 =
    Assert.assertTrue
      (IntMap.equal cmp (s1, s2))

  fun isEmptyEmptyTest () =
    Assert.assertTrue (IntMap.isEmpty IntMap.empty)

  fun memTrueTest () = 
    Assert.assertTrue
      (IntMap.mem 114 (imapFromList [(33, ~4), (114, 514)]))

  fun memFalseTest () =
    Assert.assertFalse
      (IntMap.mem 42 (imapFromList [(33, ~4), (114, 514)]))

  fun findfoundTest () = 
    Assert.assertEqualIntOption
      (SOME 514)
      (IntMap.find 114 (imapFromList [(33, ~4), (114, 514)]))

  fun findNotFoundTest () =
    Assert.assertNone
      (IntMap.find 42 (imapFromList [(33, ~4), (114, 514)]))

  fun foldElementsTest () =
    let 
      val m = imapFromList [(33, ~4), (114, 514)]
    in
      Assert.assertEqualList
        (Assert.assertEqual2Tuple
          (Assert.AssertInt.assertEqualInt, Assert.AssertInt.assertEqualInt))
        (IntMap.elements m)
        (rev (IntMap.fold (fn (k, d, l) => (k, d) :: l) m []))
    end

  fun cardinalEmptyTest () =
    Assert.AssertInt.assertEqualInt
      0
      (IntMap.cardinal IntMap.empty)

  fun cardinal2ElementsTest () =
    Assert.AssertInt.assertEqualInt
      2
      (IntMap.cardinal (imapFromList [(33, ~4), (114, 514)]))

  fun equalTest () =
    Assert.assertTrue
      (IntMap.equal op=
        (imapFromList [(3, 1), (4, 1)], imapFromList [(4, 1), (3, 1)]))

  fun removeTest () =
    assertEqualIntMap
      op=
      (IntMap.remove 1
        (IntMap.remove 4
          (imapFromList [(3, 1), (4, 1)])))
      (imapFromList [(3, 1)])

  fun splitFindFoundTest () =
    let 
      val n = 810
      val m = imapFromList [(33, ~4), (114, 514), (810, 1919)]
    in
      Assert.assertEqualIntOption
        (IntMap.find n m)
        (#data (IntMap.split n m))
    end

  fun splitFindNotFoundTest () =
    let 
      val n = 42
      val m = imapFromList [(33, ~4), (114, 514), (810, 1919)]
    in
      Assert.assertEqualIntOption
        (IntMap.find n m)
        (#data (IntMap.split n m))
    end

  fun suite () = Test.labelTests [
    ("isEmpty empty test", isEmptyEmptyTest),
    ("mem found test", memTrueTest),
    ("mem not found test", memFalseTest),
    ("find found test", findfoundTest),
    ("find not found test", findNotFoundTest),
    ("fold elements test", foldElementsTest),
    ("cardinal empty test", cardinalEmptyTest),
    ("cardinal 2 elements test", cardinal2ElementsTest),
    ("equal test", equalTest),
    ("remove test", removeTest),
    ("split find found test", splitFindFoundTest),
    ("split find not found test", splitFindNotFoundTest)
  ]
end
