structure FileSysTest =
struct
  open SMLUnit
  open FileSys
  open Assert
  structure P = OS.Path

  fun qsort cmp [] = []
    | qsort cmp (x::xs) = 
    let
        val (less, great) = List.partition (fn y => cmp(y, x) = LESS) xs
    in
        List.concat [qsort cmp less, [x], qsort cmp great]
    end

  val strSort = qsort String.compare

  val previousDir = cwd ();
  val fileSysTestDir = "./test/ext"
  val testDir = (P.concat(fileSysTestDir, "test"))
  val () = rm_rf testDir
  val () = mkdir_p testDir
  val () = cd testDir
(*
preparing this directory tree
.
├── dir1
│   ├── test1
│   ├── test2
│   └── test3
├── dir2
│   ├── test1
│   ├── test2
│   └── test3
├── dir3
│   ├── test1
│   ├── test2
│   └── test3
├── test1
├── test2
└── test3
*)

  val () = app touch ["test1", "test2", "test3"]
  val () = app (fn d => (mkdir_p d;
                        app (fn f => touch(P.concat(d, f))) ["test1", "test2", "test3"]
                       )) ["dir1", "dir2", "dir3"]


  fun suite _ = Test.labelTests [
        ("listDir: absolute",
         fn () => assertEqualStringList ["/bin/sh"] (List.filter (String.isSuffix "sh") (listDir "/"))),
        ("listDir: relative",
         fn () => assertEqualStringList ["dir1", "dir2", "dir3", "test1", "test2", "test3"]
                                        (strSort (List.filter (fn x =>
                                                                  x <> P.currentArc andalso
                                                                  x <> P.parentArc) 
                                                              (listDir "./")))),
        ("fileExists: existing file",
         fn () => assertTrue (fileExists "test1")),
        ("fileExists: non-existing file",
         fn () => assertFalse (fileExists "./non-existing-file")),
        ("fileExists: existing dir",
         fn () => assertTrue (fileExists "./")),
        ("fileExists: non-existing dir",
         fn () => assertFalse (fileExists "foo/")),
        ("expandGrob: simple string",
         fn () => assertEqualStringList [(P.mkCanonical "./")] (expandGrob "./")),
        ("expandGrob: simple string2",
         fn () => assertEqualStringList [(P.mkCanonical "./")] (expandGrob "./test1")),
        ("expandGrob: non-matching path",
         fn () => assertEqualStringList [] (expandGrob "./non-existing-file")),
        ("expandGrob: matching grob to file",
         fn () => assertEqualStringList (List.map (fn p => (P.mkAbsolute{relativeTo = cwd(), path = p} )) ["test1", "test2", "test3"]) (strSort (expandGrob "./test*"))),
        ("expandGrob: matching grob to dir",
         fn () => assertEqualStringList (List.map (fn p => (P.mkAbsolute{relativeTo = cwd(), path = p} )) ["dir1", "dir2", "dir3"]) (strSort (expandGrob "./dir*"))),
        ("expandGrob: matching grob file under grod dir",
         fn () => assertEqualStringList (List.map (fn p => (P.mkAbsolute{relativeTo = cwd(), path = p} )) ["dir1/test1", "dir1/test2", "dir1/test3",
                                                                                                           "dir2/test1", "dir2/test2", "dir2/test3",
                                                                                                           "dir3/test1", "dir3/test2", "dir3/test3"])
                                        (strSort (expandGrob "./dir*/*"))),
        ("expandGrob: non-matching grob",
         fn () => assertEqualStringList [] (expandGrob "./non*-matching")),
        ("touch: a new file",
         fn () => (touch "test4"; assertTrue (fileExists "test4"))),
        ("touch: an existing file",
         fn () => (touch "test4"; assertTrue (fileExists "test4")))
  ]
end

