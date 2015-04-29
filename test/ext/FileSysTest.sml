structure FileSysTest =
struct
  open SMLUnit
  open FileSys
  open Assert
  fun suite _ = Test.labelTests [
        ("listDir",
         fn () => assertEqualStringList ["/bin/sh"] (List.filter (String.isSuffix "sh") (listDir "/")))
  ]
end

