structure FileSys =
struct
structure F = OS.FileSys
structure P = OS.Path

fun fileExists f =
  (if F.isDir f
   then true
   else true)
  handle SysErr => false


fun listDir s =
  let
      val d = F.openDir s
      fun loop acc =
        case F.readDir d of
            SOME s => loop (s :: acc)
          | NONE => acc before F.closeDir d 
  in
      loop []
  end

fun isSubstringFrom substr str i =
  let
      datatype result = Success of int | Continue | End
      val strLen = String.size str
      val substrLen = String.size substr
      fun checkOne i j =
        if  j = substrLen
        then Success i
        else if i = strLen
        then End
        else if String.sub(str, i) = String.sub(substr, j)
        then checkOne (i+1) (j+1)
        else Continue
      fun loop i =
        case checkOne i 0 of
            Success j => SOME j
          | Continue => loop (i + 1)
          | End => NONE
  in
      loop i
  end

fun makeFilter [] wantFile = (fn str => false)
  | makeFilter (token :: tokens) wantFile =
    (fn str =>
        let
            val len = String.size str
            fun check [] i = i = len
              | check [x] i = String.isSuffix x str andalso i + String.size(x) <= len
              | check (x::xs) i = case isSubstringFrom x str i of
                                      SOME j => check xs j
                                    | NONE => false
            val i = String.size token
        in
            if wantFile
            then not (F.isDir str) andalso String.isPrefix token str andalso check tokens i
            else String.isPrefix token str andalso check tokens i
        end
    )
  

fun expandGrob path =
  let
      val cwd = F.getDir()
      val path = P.mkCanonical(P.concat(cwd, path))
      val {arcs, vol, isAbs} = P.fromString path
      val root = "/" (* if isAbs then "/" else "./" *)
      fun loop (x::xs) p =
        let
            val tokens = String.fields (fn c => c = #"*") (P.concat(p, x))
            val wantFile = xs = [] andalso x = ""
            val filter = makeFilter tokens wantFile
            fun toFullPath e = P.mkCanonical(P.concat(p, e))
            val candicates = List.filter filter (List.map toFullPath ([P.currentArc, P.parentArc] @ (listDir p)))
        in
            List.foldl (fn(e,acc) => (loop xs e) @ acc) [] candicates
        end
        | loop [] path = [path]
  in
      loop arcs root
  end
      
      
fun ascends path =
  let
      val abpath = P.mkAbsolute {path = path, relativeTo = F.getDir()}
      val can = P.mkCanonical abpath
      val {arcs, vol, isAbs} = P.fromString can
  in
      #1 (List.foldl (fn (e, (acc, prev)) =>
                         let
                             val new = P.concat(prev, e)
                         in
                             (new :: acc, new)
                         end) (["/"], "/") arcs)
  end


fun optionToStr s = Option.getOpt(s, "")

val touch =  TextIO.closeOut o TextIO.openOut

fun tmpName prefix = (optionToStr prefix) ^ (F.tmpName())

fun mkTmpDir prefix =
  let val name = tmpName prefix
  in name before F.mkDir(name)  end

fun mkTmpFile prefix =
  let val name = tmpName prefix
  in name before touch name end

fun openTmpFile prefix =
  let val name = tmpName prefix
  in (TextIO.openOut(name), name) end


fun fold f u dir =
  let
      val d = F.openDir dir
      fun loop res =
        case F.readDir d of
            SOME entry => let
             val name = (P.concat(dir, entry))
         in
             if entry = P.currentArc orelse
                entry = P.parentArc
             then loop(res)
             else if F.isDir name
             then f(name, loop(fold f res name))
             else loop(f(name, res))
         end
          | NONE => res before F.closeDir d
  in
      loop u
  end

fun fold' f u dir =
  let
      val d = F.openDir dir
      fun loop res =
        case F.readDir d of
            SOME entry => let
             val name = (P.concat(dir, entry))
         in
             if entry = P.currentArc orelse
                entry = P.parentArc
             then loop(res)
             else if F.isDir name
             then loop(f(name, fold' f res name))
             else loop(f(name, res))
         end
          | NONE => res before F.closeDir d
  in
      loop u
  end


fun walk dir f =
  fold (fn (entry, _) => f entry) () dir
fun walk' dir f =
  fold' (fn (entry, _) => f entry) () dir

fun map f dir =
  fold (fn (entry, acc) => f(entry) :: acc) [] dir

fun map' f dir =
  fold' (fn (entry, acc) => f(entry) :: acc) [] dir

fun filter f dir =
  fold (fn (entry, acc) => (if f entry
                            then entry :: acc
                            else acc)) [] dir
fun filter' f dir =
  fold' (fn (entry, acc) => (if f entry
                             then entry :: acc
                             else acc)) [] dir

val foldPreOrder = fold
val foldPostOrder = fold'
val walkPreorder = walk
val walkPostorder = walk'
val mapPreOrder = map
val mapPostOrder = map'
val filterPreOreder = filter
val filterPostOrder = filter'

fun rm_rf dir =
  (walk' dir (fn f =>
                 if F.isDir f
                 then F.rmDir f
                 else F.remove f);
   F.rmDir dir)
  handle OS.SysErr _ => ()

fun mkdir_p dir =
  let
      val {arcs,  isAbs, vol} = P.fromString dir
      (* :TODO: treat windows *)
      val unit = if isAbs
                 then "/"
                 else ""
  in
      (List.foldl (fn (entry, path) =>
                      let
                          val p = P.concat(path, entry)
                      in
                          if fileExists p
                          then p
                          else p before F.mkDir p
                      end) unit arcs);
      ()
  end

val cwd = F.getDir
val cd = F.chDir
val ls = listDir
fun mv old new = F.rename{old = old, new = new}
val rm = F.remove
val mkdir = F.mkDir
val rmdir = F.rmDir
end
