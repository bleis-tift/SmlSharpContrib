structure Stack = struct

  type 'a t = 'a list

  val empty = []
  fun isEmpty t = null t
  fun size t = size t
  fun push (x, xs) = x :: xs
  fun peekExn t = hd t
  fun peek [] = NONE
    | peek (x::_) = SOME x
  fun pop [] = NONE
    | pop (x::xs) = SOME(x, xs)
  fun popExn t = (hd t, tl t)
end

