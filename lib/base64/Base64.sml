structure Base64 =
struct
val << = Word8.<<
val >> = Word8.>>
val andb = Word8.andb
val orb = Word8.orb
val fromInt = Word8.fromInt
val toInt = Word8.toInt

val table = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
fun lookup w = String.sub(table, toInt w)
fun encode3 arr arri buf bufi =
  let
      val a = Word8Array.sub(arr, arri    )
      val b = Word8Array.sub(arr, arri + 1)
      val c = Word8Array.sub(arr, arri + 2)
  in
      CharArray.update(buf, bufi    , lookup(                            >>(a, 0w2)));
      CharArray.update(buf, bufi + 1, lookup(orb(<<(andb(a, 0w3) , 0w4), >>(b, 0w4))));
      CharArray.update(buf, bufi + 2, lookup(orb(<<(andb(b, 0w15), 0w2), >>(c, 0w6))));
      CharArray.update(buf, bufi + 3, lookup(       andb(c, 0w63)))
  end

fun encode2 arr arri buf bufi =
  let
      val a = Word8Array.sub(arr, arri    )
      val b = Word8Array.sub(arr, arri + 1)
  in
      CharArray.update(buf, bufi    , lookup(                            >>(a, 0w2)));
      CharArray.update(buf, bufi + 1, lookup(orb(<<(andb(a, 0w3) , 0w4), >>(b, 0w4))));
      CharArray.update(buf, bufi + 2, lookup(orb(<<(andb(b, 0w15), 0w2), 0w0)));
      CharArray.update(buf, bufi + 3, #"=")
  end

fun encode1 arr arri buf bufi =
  let
      val a = Word8Array.sub(arr, arri    )
  in
      CharArray.update(buf, bufi    , lookup(                            >>(a, 0w2)));
      CharArray.update(buf, bufi + 1, lookup(orb(<<(andb(a, 0w3) , 0w4), 0w0)));
      CharArray.update(buf, bufi + 2, #"=");
      CharArray.update(buf, bufi + 3, #"=")
  end

fun encode arr =
  let
      val arrLen = Word8Array.length arr
      val bufLen = ((arrLen + 2) div 3) * 4
      val buf =  CharArray.tabulate(bufLen, fn _ => chr 0)
      fun loop arri bufi = if arri < arrLen - 3
                           then (encode3 arr arri buf bufi; loop (arri+3) (bufi+4))
                           else if arri = arrLen - 3
                           then encode3 arr arri buf bufi
                           else if arri = arrLen - 2
                           then encode2 arr arri buf bufi
                           else encode1 arr arri buf bufi
  in
      loop 0 0;
      CharArray.vector buf
  end

exception InvalidChar
exception InvalidLength

fun fromChar c =
  if Char.isAlpha c
  then if Char.isUpper c
       then fromInt((ord c) - (ord #"A"))
       else fromInt((ord c) - (ord #"a") + 26)
  else if Char.isDigit c
  then fromInt((ord c) - (ord #"0") + 52)
  else if c = #"+"
  then 0w62
  else if c = #"/"
  then 0w63
  else raise InvalidChar

fun decode4 str stri buf bufi =
  let
      val w = fromChar(String.sub(str, stri    ))
      val x = fromChar(String.sub(str, stri + 1))
      val y = fromChar(String.sub(str, stri + 2))
      val z = fromChar(String.sub(str, stri + 3))
  in
      Word8Array.update(buf, bufi    , orb(<<(w, 0w2), >>(x, 0w4)));
      Word8Array.update(buf, bufi + 1, orb(<<(x, 0w4), >>(y, 0w2)));
      Word8Array.update(buf, bufi + 2, orb(<<(y, 0w6),    z))
  end

fun decode3 str stri buf bufi =
  let
      val w = fromChar(String.sub(str, stri    ))
      val x = fromChar(String.sub(str, stri + 1))
      val y = fromChar(String.sub(str, stri + 2))
  in
      Word8Array.update(buf, bufi    , orb(<<(w, 0w2), >>(x, 0w4)));
      Word8Array.update(buf, bufi + 1, orb(<<(x, 0w4), >>(y, 0w2)))
  end

fun decode2 str stri buf bufi =
  let
      val w = fromChar(String.sub(str, stri    ))
      val x = fromChar(String.sub(str, stri + 1))
  in
      Word8Array.update(buf, bufi    , orb(<<(w, 0w2), >>(x, 0w4)))
  end

fun decode str =
  let
      val strLen = String.size str
      val tailEquals = if String.sub(str, strLen - 1) <> #"="
                       then 0
                       else if String.sub(str, strLen - 2) <> #"="
                       then 1
                       else 2
      val buf = Word8Array.array((strLen div 4) * 3 - tailEquals, 0w0)
      fun loop stri bufi =
        if stri = strLen - 4
        then if tailEquals = 0
             then decode4 str stri buf bufi
             else if tailEquals  = 1
             then decode3 str stri buf bufi
             else decode2 str stri buf bufi
        else (decode4 str stri buf bufi;
              loop (stri+4) (bufi+3))
  in
      if Int.rem(strLen, 4) <> 0
      then raise InvalidLength
      else loop 0 0;
      buf
  end

fun stringToWord8Array str =
  let
      val strLen = String.size str
  in
      Word8Array.tabulate(strLen,(fn i => fromInt(ord(String.sub(str, i)))))
  end

fun word8ArrayToString arr =
  let
      val arrLen = Word8Array.length arr
  in
      CharVector.tabulate(arrLen, (fn i => chr(toInt(Word8Array.sub(arr, i)))))
  end                       
end
