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

  val iset_of_list = foldl (fn (n, s) => IntSet.add n s) IntSet.empty
  fun assertEqualIntSet s1 s2 =
    Assert.assertTrue
      (IntSet.equal (s1, s2))

  fun is_empty_empty_test () =
    Assert.assertTrue (IntSet.is_empty IntSet.empty)

  fun is_empty_singleton_test () =
    Assert.assertFalse (IntSet.is_empty (IntSet.singleton 42))

  fun mem_true_test () = 
    Assert.assertTrue
      (IntSet.mem 810 (iset_of_list [33, ~4, 114, 514, 810]))

  fun mem_false_test () =
    Assert.assertFalse
      (IntSet.mem 42 (iset_of_list [33, ~4, 114, 514, 810]))

  fun min_elt_empty_test () =
    Assert.assertNone
      (IntSet.min_elt IntSet.empty)

  fun min_elt_not_empty_test () =
    Assert.assertEqualIntOption
      (SOME ~4)
      (IntSet.min_elt (iset_of_list [33, ~4, 114, 514, 810]))

  fun max_elt_empty_test () =
    Assert.assertNone
      (IntSet.max_elt IntSet.empty)

  fun max_elt_not_empty_test () =
    Assert.assertEqualIntOption
      (SOME 810)
      (IntSet.max_elt (iset_of_list [33, ~4, 114, 514, 810]))

  fun fold_elements_test () =
    let 
      val s = iset_of_list [33, ~4, 114, 514, 810]
    in
      Assert.assertEqualList
        Assert.AssertInt.assertEqualInt
        (IntSet.elements s)
        (rev (IntSet.fold op:: s []))
    end

  fun rev_elements_test () =
    let 
      val s = iset_of_list [33, ~4, 114, 514, 810]
    in
      Assert.assertEqualList
        Assert.AssertInt.assertEqualInt
        (rev (IntSet.elements s))
        (IntSet.rev_elements s)
    end
    
  fun cardinal_empty_test () =
    Assert.AssertInt.assertEqualInt
      0
      (IntSet.cardinal IntSet.empty)

  fun cardinal_5elements_test () =
    Assert.AssertInt.assertEqualInt
      5
      (IntSet.cardinal (iset_of_list [33, ~4, 114, 514, 810]))

  fun for_all_fold_test () =
    let
      fun p n = n mod 2 = 0
      val s = iset_of_list [33, ~4, 114, 514, 810]
    in
      Assert.assertEqualBool
        (IntSet.for_all p s)
        (IntSet.fold (fn (n, b) => p n andalso b) s true)
    end

  fun exists_fold_test () =
    let
      fun p n = n mod 2 = 1
      val s = iset_of_list [114, 514, 810]
    in
      Assert.assertEqualBool
        (IntSet.exists p s)
        (IntSet.fold (fn (n, b) => p n orelse b) s false)
    end

  fun equal_test () =
    Assert.assertTrue
      (IntSet.equal
        (iset_of_list [3, 1, 4], iset_of_list [1, 4, 3]))

  fun subset_test () =
    Assert.assertTrue
      (IntSet.subset
        (iset_of_list [2, 7], iset_of_list [2, 7, 1, 8]))

  fun remove_test () =
    assertEqualIntSet
      (IntSet.remove 1
        (IntSet.remove 4
          (iset_of_list [3, 1, 4])))
      (IntSet.singleton 3)

  fun split_mem_found_test () =
    let 
      val n = 810
      val s = iset_of_list [33, ~4, 114, 514, 810]
    in
      Assert.assertEqualBool
        (IntSet.mem n s)
        (#in_ (IntSet.split n s))
    end

  fun split_mem_not_found_test () =
    let 
      val n = 42
      val s = iset_of_list [33, ~4, 114, 514, 810]
    in
      Assert.assertEqualBool
        (IntSet.mem n s)
        (#in_ (IntSet.split n s))
    end

  fun split_filter_left_test () =
    let
      val n = 810
      val s = iset_of_list [33, ~4, 114, 514, 810]
      fun p m = m < n
    in
      assertEqualIntSet
        (IntSet.filter p s)
        (#left (IntSet.split n s))
    end

  fun split_filter_right_test () =
    let
      val n = 810
      val s = iset_of_list [33, ~4, 114, 514, 810]
      fun p m = m > n
    in
      assertEqualIntSet
        (IntSet.filter p s)
        (#right (IntSet.split n s))
    end

  fun inter_test () =
    assertEqualIntSet
      (IntSet.inter
        (iset_of_list [3, 1, 4], iset_of_list [2, 7, 1]))
      (iset_of_list [1])

  fun diff_test () =
    assertEqualIntSet
      (IntSet.diff
        (iset_of_list [3, 1, 4, 1, 5, 9, 2], iset_of_list [2, 7, 1]))
      (iset_of_list [3, 4, 5, 9])

  fun union_test () =
    assertEqualIntSet
      (IntSet.union
        (iset_of_list [3, 1, 4], iset_of_list [2, 7, 1]))
      (iset_of_list [3, 1, 4, 2, 7])

  fun partition_left_test () =
    let
      fun p n = n mod 2 = 0
      val s = iset_of_list [33, ~4, 114, 514, 810]
    in
      assertEqualIntSet
        (IntSet.filter p s)
        (#1 (IntSet.partition p s))
    end
  
  fun partition_right_test () =
    let
      fun p n = n mod 2 = 0
      val s = iset_of_list [33, ~4, 114, 514, 810]
    in
      assertEqualIntSet
        (IntSet.filter (not o p) s)
        (#2 (IntSet.partition p s))
    end

  fun suite () = Test.labelTests [
    ("is_empty empty test", is_empty_empty_test),
    ("is_empty singleton test", is_empty_singleton_test),
    ("mem found test", mem_true_test),
    ("mem not found test", mem_false_test),
    ("min_elt empty test", min_elt_empty_test),
    ("min_elt not empty test", min_elt_not_empty_test),
    ("max_elt empty test", max_elt_empty_test),
    ("max_elt not_empty test", max_elt_not_empty_test),
    ("fold elements test", fold_elements_test),
    ("rev_elements test", rev_elements_test),
    ("cardinal empty test", cardinal_empty_test),
    ("cardinal 5 elements test", cardinal_5elements_test),
    ("for_all fold test", for_all_fold_test),
    ("exists fold test", exists_fold_test),
    ("equal test", equal_test),
    ("subset test", subset_test),
    ("remove test", remove_test),
    ("split mem found test", split_mem_found_test),
    ("split mem not found test", split_mem_not_found_test),
    ("split filter left test", split_filter_left_test),
    ("split filter right test", split_filter_right_test),
    ("inter test", inter_test),
    ("diff test", diff_test),
    ("union test", union_test),
    ("partition left test", partition_left_test),
    ("partition right test", partition_right_test)
  ]
end
