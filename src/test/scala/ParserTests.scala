import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Paths
import java.io.File

class LexerTest extends AnyFunSuite {
  
  val newParser = compiler.Parser;

  test("Valid Advanced Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/advanced/binarySortTree.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/advanced/hashTable.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/advanced/ticTacToe.wacc")).isSuccess)
  }

  test("Valid Array Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/array.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayBasic.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayEmpty.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayLength.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayLookup.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayNested.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayPrint.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arraySimple.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/modifyString.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/printRef.wacc")).isSuccess)
  }

  test("Valid Basic Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exit-1.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exitBasic.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exitBasic2.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exitWrap.wacc")).isSuccess) 

    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/skip/comment.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/skip/commentInLine.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/skip/skip.wacc")).isSuccess) 
  }

  test("Valid Expressions Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/andExpr.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/boolcalc.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/boolExpr1.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/charComparisonExpr.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/divExpr.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/equalsExpr.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/greaterEqExpr.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/greaterExpr.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/intCalc.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/intExpr1.wacc")).isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/lessCharExpr.wacc")).isSuccess) 
  }

  test("Valid Functions Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/fibonacciFullRec.wacc")).isSuccess)     
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/fibonacciRecursive.wacc")).isSuccess)  
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/fixedPointRealArithmetic.wacc")).isSuccess)  
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/functionalConditionalReturn.wacc")).isSuccess)  
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/mutualRecursion.wacc")).isSuccess)  
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/printInputTriangle.wacc")).isSuccess)  
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/printTriangle.wacc")).isSuccess)  
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/simpleRecursion.wacc")).isSuccess)  

    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/asciiTable.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionDeclaration.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionManyArguments.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionReturnPair.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionSimple.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionSimpleLoop.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionUpdateParameter.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/incFunction.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/negFunction.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/sameArgName.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/sameArgName2.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/sameNameAsVar.wacc")).isSuccess)
  }

  test("Valid Conditional Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if1.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if2.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if3.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if4.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if5.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if6.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/ifBasic.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/ifFalse.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/ifTrue.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/whitespace.wacc")).isSuccess) 
  }

  test("Valid IO Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/hashInProgram.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/multipleStringsAssignments.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/print-backspace.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/print-carridge-return.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/print.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printBool.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printChar.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printCharArray.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printCharAsString.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printEscChar.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printInt.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/println.wacc")).isSuccess)

    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoBigInt.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoBigNegInt.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoChar.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoInt.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoNegInt.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoPuncChar.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/read.wacc")).isSuccess)
  }

  test("Valid Pairs Test") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/checkRefPair.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/createPair.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/createPair02.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/createPair03.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/createRefPair.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/free.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/linkedList.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/nestedPair.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/null.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/printNull.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/printNullPair.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/printPair.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/printPairOfNulls.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/readPair.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/writeFst.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/writeSnd.wacc")).isSuccess)
  }
  

  test("Valid Runtime Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc")).isSuccess)

    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/divideByZero/divideByZero.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/divideByZero/divZero.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/divideByZero/modByZero.wacc")).isSuccess)

    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intJustOverflow.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intmultOverflow.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intnegateOverflow.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intnegateOverflow2.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intUnderflow.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intWayOverflow.wacc")).isSuccess)

    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/freeNull.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/readNull1.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/readNull2.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/setNull1.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/setNull2.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/useNull1.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/useNull2.wacc")).isSuccess)
  }

  test("Valid Scope Tests"){
    // assert(newParser.parseFromFile(new File("wwacc_examples/valid/scope/ifNested1.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wwacc_examples/valid/scope/ifNested2.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wwacc_examples/valid/scope/indentationNotImportant.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wwacc_examples/valid/scope/intsAndKeywords.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wwacc_examples/valid/scope/printAllTypes.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wwacc_examples/valid/scope/scope.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wwacc_examples/valid/scope/scopeBasic.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wwacc_examples/valid/scope/scopeRedefine.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wwacc_examples/valid/scope/scopeSimpleRedefine.wacc")).isSuccess)
    // assert(newParser.parseFromFile(new File("wwacc_examples/valid/scope/scopeVars.wacc")).isSuccess)
  }

  test("Valid Sequence Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/basicSeq.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/basicSeq2.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/boolAssignment.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/charAssignment.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/exitSimple.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/intAssignment.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/intLeadingZeros.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/stringAssignment.wacc")).isSuccess)
  }

  test("Valid Variables Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/_VarNames.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/boolDeclaration.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/boolDeclaration2.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/capCharDeclaration.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/charDeclaration.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/charDeclaration2.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/emptyStringDeclaration.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/intDeclaration.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/longVarNames.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/manyVariables.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/negIntDeclaration.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/puncCharDeclaration.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/stringDeclaration.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/zeroIntDeclaration.wacc")).isSuccess)
  }

  test("Valid While Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/fibonacciFullIt.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/fibonacciIterative.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/loopCharCondition.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/loopIntCondition.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/max.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/min.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/rmStyleAdd.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/rmStyleAddIO.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/whileBasic.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/whileBoolFlip.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/whileCount.wacc")).isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/whileFalse.wacc")).isSuccess)
  }


}
