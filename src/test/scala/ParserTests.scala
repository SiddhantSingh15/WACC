import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Paths
import java.io.File

class LexerTest extends AnyFunSuite {
  
  val newParser = compiler.Parser;

  test("Valid Advanced Tests") {
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/advanced/binarySortTree.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/advanced/hashTable.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/advanced/ticTacToe.wacc")).isSuccess)
  }

  test("Valid Array Tests") {
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/array/array.wacc")).isSuccess)
    // WORKS: assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayBasic.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayEmpty.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayLength.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayLookup.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayNested.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayPrint.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arraySimple.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/array/modifyString.wacc")).isSuccess)
    // WORKS: assert(newParser.parseFromFile(new File("wacc_examples/valid/array/printRef.wacc")).isSuccess)
  }

  test("Valid Basic Tests") {
    // WORKS: assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exit-1.wacc")).isSuccess) 
    // WORKS: assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exitBasic.wacc")).isSuccess) 
    // WORKS: assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exitBasic2.wacc")).isSuccess) 
    // WORKS: assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exitWrap.wacc")).isSuccess) 

    // WORKS: assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/skip/comment.wacc")).isSuccess) 
    // WORKS: assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/skip/commentInLine.wacc")).isSuccess) 
    // WORKS: assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/skip/skip.wacc")).isSuccess) 
  }

  test("Valid Expressions Tests") {
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/andExpr.wacc")).isSuccess) 
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/boolcalc.wacc")).isSuccess) 
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/boolExpr1.wacc")).isSuccess) 
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/charComparisonExpr.wacc")).isSuccess) 
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/divExpr.wacc")).isSuccess) 
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/equalsExpr.wacc")).isSuccess) 
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/greaterEqExpr.wacc")).isSuccess) 
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/greaterExpr.wacc")).isSuccess) 
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/intCalc.wacc")).isSuccess) 
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/intExpr1.wacc")).isSuccess) 
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/lessCharExpr.wacc")).isSuccess) 
  }

  test("Valid Functions Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/fibonacciFullRec.wacc")).isSuccess)     
  }

}
