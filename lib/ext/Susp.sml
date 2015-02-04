(**
  Suspension a la
  SML/NJ's SUSP http://www.smlnj.org/doc/SMLofNJ/pages/susp.html
  + Scheme's SRFI-45 http://srfi.schemers.org/srfi-45/
  + alpha
 *)
structure Susp = struct
  datatype 'a node
    = Val of 'a
    | Exn of exn
    | Lazy of (unit -> 'a t)
  withtype 'a t = 'a node ref ref

  type 'a susp = 'a t

  fun !! r = !(!r)

  fun rref x = ref (ref x)

  fun fromVal x = rref (Val x)

  fun lazy f = rref (Lazy f)

  fun delay f =
      lazy (fn () => fromVal (f ()))

  fun isEvaluated t =
      case !!t of
          Val _ => true
        | Exn _ => true
        | Lazy _ => false

  fun isVal t =
      case !!t of
          Val _ => true
        | Exn _ => false
        | Lazy _ => false

  fun isExn t =
      case !!t of
          Exn _ => true
        | Val _ => false
        | Lazy _ => false

  fun force t =
      case !!t of
          Val v => v
        | Exn e => raise e
        | Lazy f =>
          let
              val p = f () handle exn => rref (Exn exn)
          in
              if not (isEvaluated t) then
                  let
                      val content = !t
                  in
                      content := !!p;
                      p := content
                  end
              else ();
              force t
          end

  fun peek t =
      if isVal t then
          SOME (force t)
      else
          NONE

  fun map f t =
      delay (fn () => f (force t))

  fun return x =
      fromVal x

  fun bind t f =
      lazy (fn () => f (force t))

  fun join t =
      force t
end
