structure PArray = struct
  datatype 'a data = ARRAY of 'a array
                   | DIFF of int * 'a * 'a vector
  withtype 'a vector = 'a data ref

  fun rerootAux (ref (ARRAY a)) k = k a
    | rerootAux (t as ref (DIFF (i, v, t'))) k =
        rerootAux t' (fn a =>
          (t' := DIFF (i, Array.sub (a, i), t);
           Array.update (a, i, v);
           k a))
  fun reroot (ref (ARRAY a)) k = k a
    | reroot (t as ref (DIFF _)) k =
        rerootAux t (fn a => (t := ARRAY a; k a))

  val maxLen = Array.maxLen

  fun fromList l = ref (ARRAY (Array.fromList l))
  fun tabulate p = ref (ARRAY (Array.tabulate p))

  fun length t = reroot t (Array.length)
  fun sub (t, i) = reroot t (fn a => Array.sub (a, i))

  fun update (t, i, v) =
    reroot t (fn a =>
      if Array.sub (a, i) = v then t
      else
        let val result = ref (ARRAY a) in
          t := DIFF (i, Array.sub (a, i), result);
          Array.update (a, i, v);
          result
        end)

  fun mapi f t = reroot t (fn a =>
    ref (ARRAY (Array.tabulate (Array.length a, fn i =>
      f (i, Array.sub (a, i))))))
  fun map f t = mapi (fn (_, v) => f v) t

  fun appi f t = reroot t (Array.appi f)
  fun app f t = reroot t (Array.app f)
  fun foldli f a t = reroot t (Array.foldli f a)
  fun foldri f a t = reroot t (Array.foldri f a)
  fun foldl f a t = reroot t (Array.foldl f a)
  fun foldr f a t = reroot t (Array.foldr f a)
  fun findi p t = reroot t (Array.findi p)
  fun find p t = reroot t (Array.find p)
  fun exists p t = reroot t (Array.exists p)
  fun all p t = reroot t (Array.all p)
  fun collate cmp (t1, t2) =
    reroot t1 (fn a1 =>
      reroot t2 (fn a2 =>
        Array.collate cmp (a1, a2)))
end
