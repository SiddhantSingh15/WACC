import parsley.Parsley, Parsley._

class LexerTest extends org.scalatest.funsuite.AnyFunSuite {
  test("LexerParsesInteger") {
    val newParser = lexer.INTEGER;
    assert(newParser.parse("5").isSuccess)
  }
}
