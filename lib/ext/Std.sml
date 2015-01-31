val c_dump =
  _import "c_dump" : 'a array -> ()

structure Std = struct
  fun id x = x
  fun op $ (f, x) = f x
  fun flip f x y = f y x
  fun curry f x y = f (x, y)
  fun uncurry f (x, y) = f x y
  fun const x _ = x

  fun dump x =
    c_dump (Array.array (1, x))

  val version = (0,1,0)
  val version_string = "0.1.0"
end
