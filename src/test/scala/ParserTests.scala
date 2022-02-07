import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Paths
import java.io.File

class LexerTest extends AnyFunSuite {
  // test("LexerParsesInteger") {
  //   val newParser = compiler.lexer.Integer
  //   assert(newParser.parse("5").isSuccess)
  // }

  test("ParsingStuff") {
    val newParser = compiler.Parser;
    assert(newParser.parse("begin skip;skip;skip end").isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/advanced/hashTable.wacc")).isSuccess)
  }
}
