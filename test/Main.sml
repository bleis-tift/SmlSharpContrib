val suites = SMLUnit.Test.TestList [
  StdTest.suite (),
  SetTest.suite (),
  MapTest.suite (),
  QueueTest.suite (),
  StackTest.suite (),
  ParserTest.suite (),
  JsonDecoderTest.suite (),
  JsonEncoderTest.suite ()
]

val () =
  SMLUnit.TextUITestRunner.runTest {output = TextIO.stdOut} suites

val () =
  OS.Process.exit OS.Process.success
