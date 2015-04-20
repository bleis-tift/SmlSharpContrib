structure UrlTest = struct
open SMLUnit
open Url

fun id str = urlDecode(urlEncode str)
val strings = ["", "a", "ab", "a.b","a-b", "ab-_.? de+"]
fun easy_test () = Assert.assertEqualStringList strings (List.map id strings)
fun wikipedia_test () =
  (Assert.assertEqualString "%E3%82%A6%E3%82%A3%E3%82%AD%E3%83%9A%E3%83%87%E3%82%A3%E3%82%A2" (urlEncode "ウィキペディア");
   Assert.assertEqualString "ウィキペディア" (urlDecode "%E3%82%A6%E3%82%A3%E3%82%AD%E3%83%9A%E3%83%87%E3%82%A3%E3%82%A2"))
                       
fun suite _ = Test.labelTests [
      ("easy test", easy_test),
      ("test case from wikipedia", wikipedia_test)
  ]

end
