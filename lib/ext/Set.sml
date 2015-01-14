functor Set
  (X : sig 
       type t 

       val compare : t * t -> order
     end
  ) = 
struct 
  type elt = X.t
  
  datatype tree =
    LEAF
  | NODE of int * tree * X.t * tree
  
  (** val empty : tree **)
  
  val empty = LEAF
  
  (** val is_empty : tree -> bool **)
  
  fun is_empty LEAF = true
    | is_empty (NODE _) = false
  
  (** val mem : X.t -> tree -> bool **)
  
  fun mem x LEAF = false
    | mem x (NODE (_, l, k, r)) =
        case X.compare (x, k) of
          EQUAL => true
        | LESS => mem x l
        | GREATER => mem x r
  
  (** val min_elt : tree -> elt option **)
  
  fun min_elt LEAF = NONE
    | min_elt (NODE (_, LEAF, x, _)) = SOME x
    | min_elt (NODE (_, l as NODE _, x, _)) = min_elt l
  
  (** val max_elt : tree -> elt option **)
  
  fun max_elt LEAF = NONE
    | max_elt (NODE (_, _, x, LEAF)) = SOME x
    | max_elt (NODE (_, _, x, r as NODE _)) = max_elt r
  
  (** val choose : tree -> elt option **)
  
  val choose = min_elt
  
  (** val fold : (elt * 'a1 -> 'a1) -> tree -> 'a1 -> 'a1 **)
  
  fun fold f LEAF base = base
    | fold f (NODE (_, l, x, r)) base =
        fold f r (f (x, fold f l base))
  
  (** val elements_aux : X.t list -> tree -> X.t list **)
  
  fun elements_aux acc LEAF = acc
    | elements_aux acc (NODE (_, l, x, r)) =
        elements_aux (x :: elements_aux acc r) l
  
  (** val elements : tree -> X.t list **)
  
  val elements = elements_aux []
  
  (** val rev_elements_aux : X.t list -> tree -> X.t list **)
  
  fun rev_elements_aux acc LEAF = acc
    | rev_elements_aux acc (NODE (_, l, x, r)) =
        rev_elements_aux (x :: rev_elements_aux acc l) r
  
  (** val rev_elements : tree -> X.t list **)
  
  val rev_elements = rev_elements_aux []
  
  (** val cardinal : tree -> int **)
  
  fun cardinal LEAF = 0
    | cardinal (NODE (_, l, _, r)) = 1 + cardinal l + cardinal r
  
  (** val for_all : (elt -> bool) -> tree -> bool **)
  
  fun for_all f LEAF = true
    | for_all f (NODE (_, l, x, r)) =
        f x andalso for_all f l andalso for_all f r
  
  (** val exists : (elt -> bool) -> tree -> bool **)
  
  fun exists f LEAF = false
    | exists f (NODE (_, l, x, r)) =
        f x orelse exists f l orelse exists f r
  
  datatype enumeration =
    END
  | MORE of elt * tree * enumeration
  
  (** val cons : tree -> enumeration -> enumeration **)
  
  fun cons LEAF e = e
    | cons (NODE (_, l, x, r)) e =
        cons l (MORE (x, r, e))
  
  (** val compare_more :
      X.t -> (enumeration -> order) -> enumeration -> order **)
  
  fun compare_more x1 cont END = GREATER
    | compare_more x1 cont (MORE (x2, r2, e3)) =
        case X.compare (x1, x2) of
          EQUAL => cont (cons r2 e3)
        | x => x
  
  (** val compare_cont :
      tree -> (enumeration -> order) -> enumeration -> order **)
  
  fun compare_cont LEAF cont e2 = cont e2
    | compare_cont (NODE (_, l1, x1, r1)) cont e2 =
        compare_cont l1 (compare_more x1 (compare_cont r1 cont)) e2
  
  (** val compare_end : enumeration -> order **)
  
  fun compare_end END = EQUAL
    | compare_end (MORE _) = LESS
  
  (** val compare : tree * tree -> order **)
  
  fun compare (s1, s2) =
    compare_cont s1 compare_end (cons s2 END)
  
  (** val equal : tree * tree -> bool **)
  
  fun equal (s1, s2) =
    case compare (s1, s2) of
      EQUAL => true
    | _ => false
  
  (** val subsetl : (tree -> bool) -> X.t -> tree -> bool **)
  
  fun subsetl subset_l1 x1 LEAF = false
    | subsetl subset_l1 x1 (s2 as NODE (_, l2, x2, r2)) =
        case X.compare (x1, x2) of
          EQUAL => subset_l1 l2
        | LESS => subsetl subset_l1 x1 l2
        | GREATER => mem x1 r2 andalso subset_l1 s2
  
  (** val subsetr : (tree -> bool) -> X.t -> tree -> bool **)
  
  fun subsetr subset_r1 x1 LEAF = false
    | subsetr subset_r1 x1 (s2 as NODE (_, l2, x2, r2)) =
        case X.compare (x1, x2) of
          EQUAL => subset_r1 r2
        | LESS => mem x1 l2 andalso subset_r1 s2 
        | GREATER => subsetr subset_r1 x1 r2
  
  (** val subset : tree -> tree -> bool **)
  
  fun subset (LEAF, s2) = true
    | subset (s1, LEAF) = false
    | subset (NODE (_, l1, x1, r1), s2 as NODE (_, l2, x2, r2)) =
        case X.compare (x1, x2) of
          EQUAL => subset (l1, l2) andalso subset (r1, r2)
        | LESS => subsetl (fn s => subset (l1, s)) x1 l2 andalso subset (r1, s2)
        | GREATER => subsetr (fn s => subset (r1, s)) x1 r2 andalso subset (l1, s2) 
  type t = tree
  
  (** val height : tree -> int **)
  
  fun height LEAF = 0
    | height (NODE (h, _, _, _)) = h
  
  (** val singleton : X.t -> tree **)
  
  fun singleton x = NODE (1, LEAF, x, LEAF)
  
  (** val create : t -> X.t -> t -> tree **)
  
  fun create l x r =
    NODE (Int.max (height l, height r) + 1, l, x, r)
  
  (** val assert_false : t -> X.t -> t -> tree **)
  
  val assert_false = create
  
  (** val bal : t -> X.t -> t -> tree **)
  
  fun bal l x r =
    let
      val hl = height l
      val hr = height r
    in
      if hl > hr + 2 then
        case l of
          LEAF => assert_false l x r
        | NODE (_, ll, lx, lr) =>
            if height ll >= height lr then
              create ll lx (create lr x r)
            else
              case lr of
                LEAF => assert_false l x r
              | NODE (_, lrl, lrx, lrr) =>
                  create (create ll lx lrl) lrx (create lrr x r)
      else if hr > hl + 2 then
        case r of
          LEAF => assert_false l x r
        | NODE (_, rl, rx, rr) =>
            if height rr >= height rl then
              create (create l x rl) rx rr
            else
              case rl of
                LEAF => assert_false l x r
              | NODE (_, rll, rlx, rlr) =>
                  create (create l x rll) rlx (create rlr rx rr)
      else
        create l x r
    end
  
  (** val add : X.t -> tree -> tree **)
  
  fun add x LEAF = NODE (1, LEAF, x, LEAF)
    | add x (NODE (h, l, y, r)) =
        case X.compare (x, y) of
          EQUAL => NODE (h, l, y, r)
        | LESS => bal (add x l) y r
        | GREATER => bal l y (add x r)
  
  (** val join : tree -> elt -> t -> t **)
  
  fun join LEAF x = add x
    | join (l as NODE (lh, ll, lx, lr)) x =
        let
          fun join_aux LEAF = add x l
            | join_aux (r as NODE (rh, rl, rx, rr)) =
                if lh > rh + 2 then
                  bal ll lx (join lr x r)
                else if rh > lh + 2 then
                  bal (join_aux rl) rx rr
                else
                  create l x r
        in
          join_aux
        end
  
  (** val remove_min : tree -> elt -> t -> t * elt **)
  
  fun remove_min LEAF x r = (r, x)
    | remove_min (NODE (_, ll, lx, lr)) x r =
        let
          val (l', m) = remove_min ll lx lr
        in
          (bal l' x r, m)
        end
  
  (** val merge : tree * tree -> tree **)
  
  fun merge (LEAF, s2) = s2
    | merge (s1, LEAF) = s1
    | merge (s1 as NODE _, NODE (_, l2, x2, r2)) =
        let
          val (s2', m) = remove_min l2 x2 r2
        in
          bal s1 m s2'
        end
  
  (** val remove : X.t -> tree -> tree **)
  
  fun remove x LEAF = LEAF
    | remove x (NODE (_, l, y, r)) =
        case X.compare (x, y) of
          EQUAL => merge (l, r)
        | LESS => bal (remove x l) y r
        | GREATER => bal l y (remove x r)
  
  (** val concat : tree * tree -> tree **)
  
  fun concat (LEAF, s2) = s2
    | concat (s1, LEAF) = s1
    | concat (s1, NODE (_, l2, x2, r2)) =
        let
          val (s2', m) = remove_min l2 x2 r2
        in
          join s1 m s2'
        end

  (** val split : X.t -> tree -> { left : t, in : bool, right : t } **)
  
  fun split x LEAF = { left = LEAF, in_ = false, right = LEAF }
    | split x (NODE (_, l, y, r)) =
        case X.compare (x, y) of
          EQUAL => { left = l, in_ = true, right = r }
        | LESS =>
            let val { left = ll, in_ = b, right = rl } = split x l in
              { left = ll, in_ = b, right = (join rl y r) }
            end
        | GREATER =>
            let val { left = rl, in_ = b, right = rr } = split x r in
              { left = (join l y rl), in_ = b, right = rr }
            end
  
  (** val inter : tree * tree -> tree **)
  
  fun inter (LEAF, s2) = LEAF
    | inter (s1, LEAF) = LEAF
    | inter (NODE (_, l1, x1, r1), s2 as NODE _) =
        let
          val { left = l2', in_ = pres, right = r2' } = split x1 s2
        in
          if pres then
            join (inter (l1, l2')) x1 (inter (r1, r2'))
          else
            concat (inter (l1, l2'), inter (r1, r2'))
        end
  
  (** val diff : tree * tree -> tree **)
  
  fun diff (LEAF, s2) = LEAF
    | diff (s1, LEAF) = s1
    | diff (NODE (_, l1, x1, r1), s2 as NODE _) =
        let
          val { left = l2', in_ = pres, right = r2' } = split x1 s2
        in
          if pres then
            concat (diff (l1, l2'), diff (r1, r2'))
          else
            join (diff (l1, l2')) x1 (diff (r1, r2'))
        end
  
  (** val union : tree * tree -> tree **)
  
  fun union (LEAF, s2) = s2
    | union (s1, LEAF) = s1
    | union (NODE (_, l1, x1, r1), s2 as NODE _) =
        let
          val { left = l2', in_ = x, right = r2' } = split x1 s2
        in
          join (union (l1, l2')) x1 (union (r1, r2'))
        end
  
  (** val filter : (elt -> bool) -> tree -> tree **)
  
  fun filter f LEAF = LEAF
    | filter f (NODE (_, l, x, r)) =
        let
          val l' = filter f l
          val r' = filter f r
         in
           if f x then join l' x r' else concat (l', r')
         end
  
  (** val partition : (elt -> bool) -> t -> t * t **)
  
  fun partition f LEAF = (LEAF, LEAF)
    | partition f (NODE (_, l, x, r)) =
        let
          val (l1, l2) = partition f l
          val (r1, r2) = partition f r
        in
          if f x then
            (join l1 x r1, concat (l2, r2))
          else
            (concat (l1, r1), join l2 x r2)
        end
end
