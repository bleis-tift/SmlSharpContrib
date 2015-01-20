functor Map
  (X: sig 
        type t 
  
        val compare : t * t -> order
      end) =
struct 
  type key = X.t
  
  datatype 'elt t =
    LEAF
  | NODE of 'elt t * key * 'elt * 'elt t * int
  
  (** val height : 'a t -> int **)
  
  fun height LEAF = 0
    | height (NODE (_, _, _, _, h)) = h
  
  (** val cardinal : 'a t -> int **)
  
  fun cardinal LEAF = 0
    | cardinal (NODE (l, _, _, r, _)) =
        1 + cardinal l + cardinal r
  
  (** val empty : 'a t **)
  
  val empty =
    LEAF
  
  (** val isEmpty : 'a t -> bool **)
  
  fun isEmpty LEAF = true
    | isEmpty (NODE _) = false
  
  (** val mem : key -> 'a t -> bool **)
  
  fun mem x LEAF = false
    | mem x (NODE (l, y, _, r, _)) =
        case X.compare (x, y) of
          LESS => mem x l
        | EQUAL => true
        | GREATER => mem x r
  
  (** val find : key -> 'a t -> 'a option **)
  
  fun find x LEAF = NONE
    | find x (NODE (l, y, d, r, _)) =
        case X.compare (x, y) of
          LESS => find x l
        | EQUAL => SOME d
        | GREATER => find x r
  
  (** val create : 'a t -> key -> 'a -> 'a t -> 'a t **)
  
  fun create l x e r =
    NODE (l, x, e, r, Int.max (height l, height r) + 1)
  
  (** val assertFalse : 'a t -> key -> 'a -> 'a t -> 'a t **)
  
  val assertFalse =
    create
  
  (** val bal : 'a t -> key -> 'a -> 'a t -> 'a t **)
  
  fun bal l x d r =
    let
      val hl = height l
      val hr = height r
    in
      if hl > hr + 2 then
        case l of
          LEAF => assertFalse l x d r
        | NODE (ll, lx, ld, lr, _) =>
            if height ll >= height lr then
              create ll lx ld (create lr x d r)
            else
              case lr of
                LEAF => assertFalse l x d r
              | NODE (lrl, lrx, lrd, lrr, _) =>
                  create (create ll lx ld lrl) lrx lrd (create lrr x d r)
      else
        if hr > hl + 2 then
          case r of
            LEAF => assertFalse l x d r
          | NODE (rl, rx, rd, rr, _) =>
              if height rr >= height rl then
                create (create l x d rl) rx rd rr
              else
                case rl of
                  LEAF => assertFalse l x d r
                | NODE (rll, rlx, rld, rlr, _) =>
                    create (create l x d rll) rlx rld (create rlr rx rd rr)
        else
          create l x d r
    end
  
  (** val add : key -> 'a -> 'a t -> 'a t **)
  
  fun add x d LEAF = NODE (LEAF, x, d, LEAF, 1)
    | add x d (NODE (l, y, d', r, h)) =
        case X.compare (x, y) of
          LESS => bal (add x d l) y d' r
        | EQUAL => NODE (l, y, d, r, h)
        | GREATER => bal l y d' (add x d r)
  
  (** val removeMin :
      'a t -> key -> 'a -> 'a t -> 'a t * (key * 'a) **)
  
  fun removeMin LEAF x d r = (r, (x, d))
    | removeMin (NODE (ll, lx, ld, lr, _)) x d r =
        let
          val (l', m) = removeMin ll lx ld lr
        in 
          (bal l' x d r, m)
        end
  
  (** val merge : 'a t -> 'a t -> 'a t **)
  
  fun merge LEAF s2 = s2
    | merge s1 LEAF = s1
    | merge (s1 as NODE _) (NODE (l2, x2, d2, r2, _)) =
        let
          val (s2', (x, d)) = removeMin l2 x2 d2 r2
        in
          bal s1 x d s2'
        end
  
  (** val remove : key -> 'a t -> 'a t **)
  
  fun remove x LEAF = LEAF
    | remove x (NODE (l, y, d, r, _)) =
        case X.compare (x, y) of
          LESS => bal (remove x l) y d r
        | EQUAL => merge l r
        | GREATER => bal l y d (remove x r)
  
  (** val join : 'a t -> key -> 'a -> 'a t -> 'a t **)
  
  fun join LEAF x d m = add x d m
    | join (l as NODE (ll, lx, ld, lr, lh)) x d m =
        let
          fun joinAux LEAF = add x d l
            | joinAux (r as NODE (rl, rx, rd, rr, rh)) =
                if lh > rh + 2 then
                  bal ll lx ld (join lr x d r)
                else if rh > lh + 2 then
                  bal (joinAux rl) rx rd rr
                else
                  create l x d r
        in
          joinAux m
        end
  
  (** val split : key -> 'a t -> { less : 'a t, data : 'elt option, greater : 'a t } **)
  
  fun split x LEAF = { less = LEAF, data = NONE, greater = LEAF }
    | split x (NODE (l, y, d, r, _)) =
        case X.compare (x, y) of
          LESS =>
            let
              val { less = ll, data = o0, greater = rl } = split x l
            in
              { less = ll, data = o0, greater = join rl y d r }
            end
        | EQUAL => { less = l, data = SOME d, greater = r }
        | GREATER =>
            let
              val { less = rl, data = o0, greater = rr } = split x r
            in
              { less = join l y d rl, data = o0, greater = rr }
            end
  
  (** val concat : 'a t -> 'a t -> 'a t **)
  
  fun concat LEAF m2 = m2
    | concat m1 LEAF = m1
    | concat (m1 as NODE _) (NODE (l2, x2, d2, r2, t3)) =
        let
          val (m2', xd) = removeMin l2 x2 d2 r2
        in
          join m1 (#1 xd) (#2 xd) m2'
        end
  
  (** val elementsAux : (key * 'a) list -> 'a t -> (key * 'a) list **)
  
  fun elementsAux acc LEAF = acc
    | elementsAux acc (NODE (l, x, d, r, _)) =
        elementsAux ((x, d) :: elementsAux acc r) l
  
  (** val elements : 'a t -> (key * 'a) list **)
  
  fun elements m =
    elementsAux [] m
  
  (** val fold : (key * 'a * 'b -> 'b) -> 'a t -> 'b -> 'b **)
  
  fun fold f LEAF a = a
    | fold f (NODE (l, x, d, r, t0)) a =
        fold f r (f (x, d, fold f l a))
  
  datatype 'elt enumeration =
    END
  | MORE of key * 'elt * 'elt t * 'elt enumeration
  
  (** val cons : 'a t -> 'a enumeration -> 'a enumeration **)
  
  fun cons LEAF e = e
    | cons (NODE (l, x, d, r, h)) e = cons l (MORE (x, d, r, e))
  
  (** val equalMore :
      ('a * 'a -> bool) -> key -> 'a -> ('a enumeration -> bool) -> 'a
      enumeration -> bool **)
  
  fun equalMore cmp x1 d1 cont END = false
    | equalMore cmp x1 d1 cont (MORE (x2, d2, r2, e3)) =
        case X.compare (x1, x2) of
          EQUAL =>
            cmp (d1, d2) andalso cont (cons r2 e3)
        | _ => false
  
  (** val equalCont :
      ('a * 'a -> bool) -> 'a t -> ('a enumeration -> bool) -> 'a
      enumeration -> bool **)
  
  fun equalCont cmp LEAF cont e2 = cont e2
    | equalCont cmp (NODE (l1, x1, d1, r1, t0)) cont e2 =
        equalCont cmp l1 (equalMore cmp x1 d1 (equalCont cmp r1 cont)) e2
  
  (** val equalEnd : 'a enumeration -> bool **)
  
  fun equalEnd END = true
    | equalEnd (MORE _) = false
  
  (** val equal : ('a * 'a -> bool) -> 'a t * 'a t -> bool **)
  
  fun equal cmp (m1, m2) =
    equalCont cmp m1 equalEnd (cons m2 END)
  
  (** val map : ('a -> 'b) -> 'a t -> 'b t **)
  
  fun map f LEAF = LEAF
    | map f (NODE (l, x, d, r, h)) =
        NODE (map f l, x, f d, map f r, h)
  
  (** val mapi : (key * 'a -> 'b) -> 'a t -> 'b t **)
  
  fun mapi f LEAF = LEAF
    | mapi f (NODE (l, x, d, r, h)) =
        NODE (mapi f l, x, f (x, d), mapi f r, h)
  
  (** val mapPartial : (key * 'a -> 'b option) -> 'a t -> 'b t **)
  
  fun mapPartial f LEAF = LEAF
    | mapPartial f (NODE (l, x, d, r, _)) =
        case f (x, d) of
          SOME d' =>
            join (mapPartial f l) x d' (mapPartial f r)
        | NONE =>
            concat (mapPartial f l) (mapPartial f r)
  
  (** val map2Opt :
      (key * 'a * 'b option -> 'c option) -> ('a t -> 'c t) ->
      ('b t -> 'c t) -> 'a t * 'b t -> 'c t **)
  
  fun map2Opt f mapl mapr (LEAF, m2) = mapr m2
    | map2Opt f mapl mapr (m1, LEAF) = mapl m1
    | map2Opt f mapl mapr (NODE (l1, x1, d1, r1, h1), m2 as NODE (t0, k, y, t1, t2)) =
        let
          val { less = l2', data = o2, greater = r2' } = split x1 m2
        in
          case f (x1, d1, o2) of
            SOME e =>
              join (map2Opt f mapl mapr (l1, l2')) x1 e
                (map2Opt f mapl mapr (r1, r2'))
          | NONE =>
              concat (map2Opt f mapl mapr (l1, l2'))
                (map2Opt f mapl mapr (r1, r2'))
        end
  
  (** val pairMap :
      ('a option * 'b option -> 'c option) -> 'a t * 'b t -> 'c t **)
  
  fun pairMap f =
    map2Opt (fn (x, d, o0) => f (SOME d, o0))
      (mapPartial (fn (x, d) => f (SOME d, NONE)))
      (mapPartial (fn (x, d') => f (NONE, SOME d')))
end
