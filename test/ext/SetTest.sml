structure SetTest =
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

  structure IntSet = Set (OrderedInt)

  val isetFromList = foldl (fn (n, s) => IntSet.add n s) IntSet.empty
  fun assertEqualIntSet s1 s2 =
    Assert.assertTrue
      (IntSet.equal (s1, s2))

  fun isEmptyEmptyTest () =
    Assert.assertTrue (IntSet.isEmpty IntSet.empty)

  fun isEmptySingletonTest () =
    Assert.assertFalse (IntSet.isEmpty (IntSet.singleton 42))

  fun mem_trueTest () = 
    Assert.assertTrue
      (IntSet.mem 810 (isetFromList [33, ~4, 114, 514, 810]))

  fun mem_falseTest () =
    Assert.assertFalse
      (IntSet.mem 42 (isetFromList [33, ~4, 114, 514, 810]))

  fun minEltEmptyTest () =
    Assert.assertNone
      (IntSet.minElt IntSet.empty)

  fun minEltNotEmptyTest () =
    Assert.assertEqualIntOption
      (SOME ~4)
      (IntSet.minElt (isetFromList [33, ~4, 114, 514, 810]))

  fun maxEltEmptyTest () =
    Assert.assertNone
      (IntSet.maxElt IntSet.empty)

  fun maxEltNotEmptyTest () =
    Assert.assertEqualIntOption
      (SOME 810)
      (IntSet.maxElt (isetFromList [33, ~4, 114, 514, 810]))

  fun foldElementsTest () =
    let 
      val s = isetFromList [33, ~4, 114, 514, 810]
    in
      Assert.assertEqualList
        Assert.AssertInt.assertEqualInt
        (IntSet.elements s)
        (rev (IntSet.fold op:: s []))
    end

  fun revElementsTest () =
    let 
      val s = isetFromList [33, ~4, 114, 514, 810]
    in
      Assert.assertEqualList
        Assert.AssertInt.assertEqualInt
        (rev (IntSet.elements s))
        (IntSet.revElements s)
    end
    
  fun cardinalEmptyTest () =
    Assert.AssertInt.assertEqualInt
      0
      (IntSet.cardinal IntSet.empty)

  fun cardinal5ElementsTest () =
    Assert.AssertInt.assertEqualInt
      5
      (IntSet.cardinal (isetFromList [33, ~4, 114, 514, 810]))

  fun forallFoldTest () =
    let
      fun p n = n mod 2 = 0
      val s = isetFromList [33, ~4, 114, 514, 810]
    in
      Assert.assertEqualBool
        (IntSet.forall p s)
        (IntSet.fold (fn (n, b) => p n andalso b) s true)
    end

  fun existsFoldTest () =
    let
      fun p n = n mod 2 = 1
      val s = isetFromList [114, 514, 810]
    in
      Assert.assertEqualBool
        (IntSet.exists p s)
        (IntSet.fold (fn (n, b) => p n orelse b) s false)
    end

  fun equalTest () =
    Assert.assertTrue
      (IntSet.equal
        (isetFromList [3, 1, 4], isetFromList [1, 4, 3]))

  fun subsetTest () =
    Assert.assertTrue
      (IntSet.subset
        (isetFromList [2, 7], isetFromList [2, 7, 1, 8]))

  fun removeTest () =
    assertEqualIntSet
      (IntSet.remove 1
        (IntSet.remove 4
          (isetFromList [3, 1, 4])))
      (IntSet.singleton 3)

  fun splitMemFoundTest () =
    let 
      val n = 810
      val s = isetFromList [33, ~4, 114, 514, 810]
    in
      Assert.assertEqualBool
        (IntSet.mem n s)
        (#present (IntSet.split n s))
    end

  fun splitMemNotFoundTest () =
    let 
      val n = 42
      val s = isetFromList [33, ~4, 114, 514, 810]
    in
      Assert.assertEqualBool
        (IntSet.mem n s)
        (#present (IntSet.split n s))
    end

  fun splitFilterLessTest () =
    let
      val n = 810
      val s = isetFromList [33, ~4, 114, 514, 810]
      fun p m = m < n
    in
      assertEqualIntSet
        (IntSet.filter p s)
        (#less (IntSet.split n s))
    end

  fun splitFilterGreaterTest () =
    let
      val n = 810
      val s = isetFromList [33, ~4, 114, 514, 810]
      fun p m = m > n
    in
      assertEqualIntSet
        (IntSet.filter p s)
        (#greater (IntSet.split n s))
    end

  fun interTest () =
    assertEqualIntSet
      (IntSet.inter
        (isetFromList [3, 1, 4], isetFromList [2, 7, 1]))
      (isetFromList [1])

  fun diffTest () =
    assertEqualIntSet
      (IntSet.diff
        (isetFromList [3, 1, 4, 1, 5, 9, 2], isetFromList [2, 7, 1]))
      (isetFromList [3, 4, 5, 9])

  fun unionTest () =
    assertEqualIntSet
      (IntSet.union
        (isetFromList [3, 1, 4], isetFromList [2, 7, 1]))
      (isetFromList [3, 1, 4, 2, 7])

  fun partitionSatisfyTest () =
    let
      fun p n = n mod 2 = 0
      val s = isetFromList [33, ~4, 114, 514, 810]
    in
      assertEqualIntSet
        (IntSet.filter p s)
        (#satisfy (IntSet.partition p s))
    end
  
  fun partitionDissatisfyTest () =
    let
      fun p n = n mod 2 = 0
      val s = isetFromList [33, ~4, 114, 514, 810]
    in
      assertEqualIntSet
        (IntSet.filter (not o p) s)
        (#dissatisfy (IntSet.partition p s))
    end

  fun suite () = Test.labelTests [
    ("isEmpty empty test", isEmptyEmptyTest),
    ("isEmpty singleton test", isEmptySingletonTest),
    ("mem found test", mem_trueTest),
    ("mem not found test", mem_falseTest),
    ("minElt empty test", minEltEmptyTest),
    ("minElt not empty test", minEltNotEmptyTest),
    ("maxElt empty test", maxEltEmptyTest),
    ("maxElt not empty test", maxEltNotEmptyTest),
    ("fold elements test", foldElementsTest),
    ("revElements test", revElementsTest),
    ("cardinal empty test", cardinalEmptyTest),
    ("cardinal 5 elements test", cardinal5ElementsTest),
    ("forall fold test", forallFoldTest),
    ("exists fold test", existsFoldTest),
    ("equal test", equalTest),
    ("subset test", subsetTest),
    ("remove test", removeTest),
    ("split mem found test", splitMemFoundTest),
    ("split mem not found test", splitMemNotFoundTest),
    ("split filter less test", splitFilterLessTest),
    ("split filter greater test", splitFilterGreaterTest),
    ("inter test", interTest),
    ("diff test", diffTest),
    ("union test", unionTest),
    ("partition left test", partitionSatisfyTest),
    ("partition right test", partitionDissatisfyTest)
  ]
end
