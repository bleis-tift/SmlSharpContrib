structure FileSys =
struct
structure F = OS.FileSys
structure O = Posix.FileSys

val cwd = F.getDir

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

fun walk dir f =
  fold (fn (entry, _) => f entry) () dir

fun map f dir =
  fold (fn (entry, acc) => (f(entry) :: acc)) [] dir

fun filter f dir =
  fold (fn (entry, acc) => (if f entry
                            then entry :: acc
                            else acc)) [] dir
end
