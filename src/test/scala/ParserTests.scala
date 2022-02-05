import parsley.Parsley, Parsley._
import org.scalatest.funsuite.AnyFunSuite

class LexerTest extends AnyFunSuite {
  test("LexerParsesInteger") {
    val newParser = compiler.lexer.INTEGER;
    assert(newParser.parse("5").isSuccess)
  }
}
