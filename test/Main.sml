val suites = SMLUnit.Test.TestList [
  StdTest.suite (),
  SetTest.suite (),
  QueueTest.suite (),
  StackTest.suite (),
  ParserTest.suite ()
]

val () =
  SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suites

val () =
  OS.Process.exit OS.Process.success
