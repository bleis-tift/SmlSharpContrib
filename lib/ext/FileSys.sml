structure FileSys =
struct
structure F = OS.FileSys
structure O = Posix.FileSys

fun fileExists f =
  (if F.isDir f
   then true
   else true)
  handle SysErr => false


fun listDir s =
  let
      val d = F.openDir s;
      fun loop acc =
        case F.readDir d of
            SOME s => loop (s :: acc)
          | NONE => (F.closeDir d; acc)
  in
      loop []
  end

datatype result
  = Some of int
  | None
  | End

fun isSubstringFrom substr str i =
  let
      val strLen = String.size str
      val substrLen = String.size substr
      fun checkOne i j =
        if  j = substrLen
        then Some i
        else if i = strLen
        then End
        else if String.sub(str, i) = String.sub(substr, j)
        then checkOne (i+1) (j+1)
        else None
      fun loop i =
        case checkOne i 0 of
            Some j => SOME j
          | None => loop (i + 1)
          | End => NONE
  in
      loop i
  end

fun makeFilter tokens wantFile =
  (fn str =>
      let
          fun check [] _ = true
            | check [x] _ = String.isSuffix x str
            | check (x::xs) i = case isSubstringFrom x str i of
                                    SOME i => check xs i
                                  | NONE => false
          val (x::xs) = tokens
          val i = String.size x
      in
          if wantFile
          then not (F.isDir str) andalso (String.isPrefix x str) andalso (check xs i)
          else (String.isPrefix x str) andalso (check xs i)
                                                   
      end
  )

fun expandGrob path =
  let
      val {arcs, vol, isAbs} = OS.Path.fromString path
      val root = if isAbs then "/" else "./"
      fun loop (x::xs) path =
        let
            val tokens = String.fields (fn c => c = #"*") x
            val wantFile = xs = []
            val filter = makeFilter tokens wantFile
            val candicates = if wantFile
                             then List.filter filter [path]
                             else List.filter filter 
                                              (List.map (fn e => OS.Path.concat(path, e))
                                                        (listDir path))
        in
            List.foldl (fn(e,acc) => (loop xs e) @ acc) [] candicates
        end
        | loop [] path = [path]
  in
      loop arcs root
  end
      
      

fun optionToStr (SOME s) = s
  | optionToStr NONE = ""

fun tmpName prefix = (optionToStr prefix) ^ (F.tmpName())

fun mkTmpDir prefix =
  let val name = tmpName prefix
  in (F.mkDir(name); name) end

fun mkTmpFile prefix =
  let val name = tmpName prefix
  in (O.creat(name, O.S.irwxu); name) end

fun openTmpFile prefix =
  let val name = tmpName prefix
  in (TextIO.openOut(name), name) end

fun fold f u dir =
  let
      val d = F.openDir dir;
      fun loop res =
        case F.readDir d of
            SOME entry => let
             val name = (OS.Path.concat(dir, entry))
         in
             if F.isDir name
             then loop(fold f res name)
             else loop(f(name, res))
         end
          | NONE => (F.closeDir d; res)
  in
      loop u
  end

fun fold' f u dir =
  let
      val d = F.openDir dir;
      fun loop res =
        case F.readDir d of
            SOME entry => let
             val name = (OS.Path.concat(dir, entry))
         in
             if F.isDir name
             then loop(f(name, fold' f res name))
             else loop(f(name, res))
         end
          | NONE => (F.closeDir d; res)
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

fun rm_rf dir =
  (walk' dir (fn f =>
                 if F.isDir f
                 then F.rmDir f
                 else F.remove f);
   F.rmDir dir)

fun mkdir_p dir =
  let
      val {arcs,  isAbs, vol} = OS.Path.fromString dir
      (* :TODO: treat windows *)
      val unit = if isAbs
                 then "/"
                 else ""
  in
      List.foldl (fn (entry, path) =>
                     let
                         val p = OS.Path.concat(path, entry)
                     in
                         if fileExists p
                         then p
                         else (F.mkDir p; p)
                     end) unit arcs
  end

val cwd = F.getDir
val ls = listDir
val mv = F.rename
val rm = F.remove
val mkdir = F.mkDir
end
