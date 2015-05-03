structure Show =
struct
type 'a show = 'a -> string

fun showUnit () = "()"
val showInt = Int.toString
fun showWord w = "0wx" ^ (Word.toString w)
fun showWord8 w = "0wx" ^ (Word8.toString w)
fun showWord32 w = "0wx" ^ (Word32.toString w)
val showReal = Real.toString
fun showChar c = "#\"" ^ (Char.toString c) ^ "\""
fun showString x = "\"" ^ (String.translate
                               (fn c =>
                                   case c of
                                       #"\\" => "\\\\"
                                     | #"\"" => "\\\""
                                     | x => Char.toString x
                               )
                               x)
                   ^ "\""
fun showSubstring x = "<Substring: \"" ^ (showString (Substring.string x)) ^ "\">"
val showBool = Bool.toString

val showException = exnName

fun showRef show a = "ref " ^ (show (!a))
val      showUnitRef = showRef showUnit
val       showIntRef = showRef showInt
val      showWordRef = showRef showWord
val     showWord8Ref = showRef showWord8
val    showWord32Ref = showRef showWord32
val      showRealRef = showRef showReal
val      showCharRef = showRef showChar
val    showStringRef = showRef showString
val showSubstringRef = showRef showSubstring
val      showBoolRef = showRef showBool

fun showOption show NONE = "NONE"
  | showOption show (SOME x) = "SOME " ^ (show x)
val      showUnitOption = showOption showUnit
val       showIntOption = showOption showInt
val      showWordOption = showOption showWord
val     showWord8Option = showOption showWord8
val    showWord32Option = showOption showWord32
val      showRealOption = showOption showReal
val      showCharOption = showOption showChar
val    showStringOption = showOption showString
val showSubstringOption = showOption showSubstring
val      showBoolOption = showOption showBool

fun showOrder GREATER = "GREATER"
  | showOrder EQUAL   = "EQUAL"
  | showOrder LESS    = "LESS"

fun show2Tuple
        ((show1, show0))
        ((x1, x0))
  =
  String.concat ["(",
                 show1 x1, ", ",
                 show0 x0,
                 ")"]

fun show3Tuple
        ((show2, show1, show0))
        ((x2, x1, x0))
  =
  String.concat ["(",
                 show2 x2, ", ",
                 show1 x1, ", ",
                 show0 x0,
                 ")"]

fun show4Tuple
        ((show3, show2, show1, show0))
        ((x3, x2, x1, x0))
  =
  String.concat ["(",
                 show3 x3, ", ",
                 show2 x2, ", ",
                 show1 x1, ", ",
                 show0 x0,
                 ")"]

fun show5Tuple
        ((show4, show3, show2, show1, show0))
        ((x4, x3, x2, x1, x0))
  =
  String.concat ["(",
                 show4 x4, ", ",
                 show3 x3, ", ",
                 show2 x2, ", ",
                 show1 x1, ", ",
                 show0 x0,
                 ")"]

fun show6Tuple
        ((show5, show4, show3, show2, show1, show0))
        ((x5, x4, x3, x2, x1, x0))
  =
  String.concat ["(",
                 show5 x5, ", ",
                 show4 x4, ", ",
                 show3 x3, ", ",
                 show2 x2, ", ",
                 show1 x1, ", ",
                 show0 x0,
                 ")"]

fun show7Tuple
        ((show6, show5, show4, show3, show2, show1, show0))
        ((x6, x5, x4, x3, x2, x1, x0))
  =
  String.concat ["(",
                 show6 x6, ", ",
                 show5 x5, ", ",
                 show4 x4, ", ",
                 show3 x3, ", ",
                 show2 x2, ", ",
                 show1 x1, ", ",
                 show0 x0,
                 ")"]

fun show8Tuple
        ((show7, show6, show5, show4, show3, show2, show1, show0))
        ((x7, x6, x5, x4, x3, x2, x1, x0))
  =
  String.concat ["(",
                 show7 x7, ", ",
                 show6 x6, ", ",
                 show5 x5, ", ",
                 show4 x4, ", ",
                 show3 x3, ", ",
                 show2 x2, ", ",
                 show1 x1, ", ",
                 show0 x0,
                 ")"]

fun show9Tuple
        ((show8, show7, show6, show5, show4, show3, show2, show1, show0))
        ((x8, x7, x6, x5, x4, x3, x2, x1, x0))
  =
  String.concat ["(",
                 show8 x8, ", ",
                 show7 x7, ", ",
                 show6 x6, ", ",
                 show5 x5, ", ",
                 show4 x4, ", ",
                 show3 x3, ", ",
                 show2 x2, ", ",
                 show1 x1, ", ",
                 show0 x0,
                 ")"]

fun show10Tuple
        ((show9, show8, show7, show6, show5, show4, show3, show2, show1, show0))
        ((x9, x8, x7, x6, x5, x4, x3, x2, x1, x0))
  =
  String.concat ["(",
                 show9 x9, ", ",
                 show8 x8, ", ",
                 show7 x7, ", ",
                 show6 x6, ", ",
                 show5 x5, ", ",
                 show4 x4, ", ",
                 show3 x3, ", ",
                 show2 x2, ", ",
                 show1 x1, ", ",
                 show0 x0,
                 ")"]

fun showVector show v = String.concat(
      "#<Vector: " ::
      (List.tl (Vector.foldr
                    (fn (x, acc) => ", " :: (show x) :: acc)
                    [">"] v)))
val      showWordVector = showVector showWord
val     showWord8Vector = showVector showWord8
val      showCharVector = showVector showChar

fun showArray show a = String.concat(
      "#<Array: " ::
      (List.tl (Array.foldr
                    (fn (x, acc) => ", " :: (show x) :: acc)
                    [">"] a)))
val      showWordArray = showArray showWord
val     showWord8Array = showArray showWord8
val      showCharArray = showArray showChar

fun showList show a =  String.concat(
      "[" ::
      (List.tl (List.foldr
                    (fn (x, acc) => ", " :: (show x) :: acc)
                    ["]"] a)))
val      showUnitList = showList showUnit
val       showIntList = showList showInt
val      showWordList = showList showWord
val     showWord8List = showList showWord8
val    showWord32List = showList showWord32
val      showRealList = showList showReal
val      showCharList = showList showChar
val    showStringList = showList showString
val showSubstringList = showList showSubstring
val      showBoolList = showList showBool
end
