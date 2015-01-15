structure Parser = struct

  type 'a t = int -> ('a * int) option

  fun orElse a b =
    fn pos =>
      case a pos of
        NONE => b pos
        | p as SOME _ => p

  fun andThen a b =
    fn pos =>
      Option.mapPartial (fn (r1, rpos1) =>
        Option.map (fn (r2, rpos2) =>
          ((r1, r2), rpos2)) (b rpos1)) (a pos)

  fun map f p =
    fn pos =>
      Option.map (fn (r, rpos) =>
        (f r, rpos)) (p pos)

  fun mapPartial f p =
    fn pos =>
      Option.mapPartial (fn (r, rpos) =>
        (f r) rpos) (p pos)

  fun opt p =
    fn pos =>
      case p pos of
        NONE => SOME(NONE, pos)
        | SOME (r, rpos) => SOME(SOME r, rpos)

  fun many p =
    let
      fun parse acc pos =
        case p pos of
          SOME(r, rpos1) => parse (r::acc) rpos1
          | NONE => (List.rev acc, pos)
    in
      fn pos => SOME(parse [] pos)
    end

  fun many1 p =
    map (fn (p1, p2) => p1::p2) (andThen p (many p))

  fun not p =
    fn pos =>
      case p pos of
        SOME _ => NONE
        | NONE => SOME((), pos)

  fun pand p = not (not p)

  fun pstring s input =
    fn pos =>
      if String.size input <= pos then NONE
      else
        let
          val i = String.extract (input, pos, NONE)
        in
          if String.isPrefix s i then SOME(s, pos + String.size s)
          else NONE
        end

  fun any input =
    fn pos =>
      if String.size input <= pos then NONE
      else
        let
          val i = String.extract (input, pos, NONE)
        in
          if String.size i > 0 then
            SOME(String.substring (i, 0, 1), pos + 1)
          else NONE
        end

  fun parse p input =
    Option.map (fn (r, pos) =>
      (r, String.extract (input, pos, NONE))) (p 0)
end

