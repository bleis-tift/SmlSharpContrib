structure LIST_IMPL = struct
  fun sortWith cmp xs =
    let
      fun sequences (a::b::xs) =
          if cmp (a, b) = GREATER then
            descending b [a] xs
          else
            ascending b (fn lst => a::lst) xs
        | sequences xs = [xs]
   
      and descending x xs (y::ys) =
          if cmp (x, y) = GREATER then
            descending y (x::xs) ys
          else
            (x::xs)::(sequences (y::ys))
        | descending x xs [] =
            (x::xs)::([[]])
   
      and ascending x xs (y::ys) =
          if cmp (x, y) <> GREATER then
            ascending y (fn lst => xs (x::lst)) ys
          else
            (xs [x])::(sequences (y::ys))
        | ascending x xs [] =
            (xs [x])::([[]])
   
      fun mergeAll [x] = x
        | mergeAll xs = mergeAll (mergePairs xs)
   
      and mergePairs (a::b::xs) = (merge a b)::(mergePairs xs)
        | mergePairs xs = xs
   
      and merge [] ys = ys
        | merge xs [] = xs
        | merge (xs as x::xs') (ys as y::ys') =
            if cmp (x, y) = GREATER then
              y::(merge xs ys')
            else
              x::(merge xs' ys)
    in
      mergeAll (sequences xs)
    end
end
