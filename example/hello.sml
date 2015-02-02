datatype 'a s = Foo of 'a | Bar

val () =
  Std.dump 42;
  Std.dump "foo";
  Std.dump { x = 10, y = 42 };
  Std.dump (Foo { x = true })
