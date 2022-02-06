import org.scalatest.funsuite.AnyFunSuite

class LexerTest extends AnyFunSuite {
  // test("LexerParsesInteger") {
  //   val newParser = compiler.lexer.Integer
  //   assert(newParser.parse("5").isSuccess)
  // }

  test("ParsingStuff") {
    val newParser = compiler.Parser;
    assert(newParser.parse("begin skip end").isSuccess)
  }
}
