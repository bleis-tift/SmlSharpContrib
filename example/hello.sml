type t = { x : int, y : int }
datatype s = Foo | Bar

val () =
  Std.dump 42;
  Std.dump "foo";
  Std.dump { x = 10, y = 42 };
  Std.dump Foo
