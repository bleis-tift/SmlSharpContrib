structure Queue = struct

  datatype 'a t = Queue of 'a list * int * 'a list * int
  exception Empty

  val empty = Queue([], 0, [], 0)

  fun isEmpty (Queue(_, l, _, _)) = l = 0

  fun size (Queue(_, l, _, r)) = l + r

  fun queue (l, lLength, r, rLength) =
    if rLength < lLength then Queue(l, lLength, r, rLength)
    else Queue(l @ List.rev r, lLength + rLength, [], 0)

  fun enqueue (Queue(l, lLength, r, rLength), x) =
    queue(l, lLength, x::r, rLength + 1)

  fun peek (Queue([], _, _, _)) = NONE
    | peek (Queue(x::xs, _, _, _)) = SOME x

  fun peekExn q =
    case peek q of
      SOME t => t
      | NONE => raise Empty

  fun dequeue (Queue([], _, _, _)) = NONE
    | dequeue (Queue(x::xs, lLength, r, rLength)) =
      SOME(x, queue(xs, lLength - 1, r, rLength))

  fun dequeueExn q =
    case dequeue q of
      SOME v => v
      | NONE => raise Empty

  fun map f (Queue(l, lLength, r, rLength)) =
    let
      val nl = List.map f l
      val nr = List.foldr (fn (x, xs) => f x :: xs) [] r
    in
      Queue(nl, lLength, nr, rLength)
    end

  fun app f (Queue(l, _, r, _)) =
    (List.app f l;
    List.foldr (fn (x, ()) => f x) () r)

  fun fold f seed (Queue(l, _, r, _)) =
    List.foldr f (List.foldl f seed l) r
end

