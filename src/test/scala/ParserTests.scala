import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Paths
import java.io.File
import parsley.Success
import parsley.Failure
import frontend._

class ParserTests extends AnyFunSuite {
  
  val newParser = frontend.Parser;

  test("Valid Advanced Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/advanced/binarySortTree.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/advanced/hashTable.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/advanced/ticTacToe.wacc")).get.isSuccess)
  }

  test("Valid Array Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/array.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayBasic.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayEmpty.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayLength.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayLookup.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayNested.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arrayPrint.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/arraySimple.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/modifyString.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/array/printRef.wacc")).get.isSuccess)
  }

  test("Valid Basic Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exit-1.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exitBasic.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exitBasic2.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/exit/exitWrap.wacc")).get.isSuccess) 

    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/skip/comment.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/skip/commentInLine.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/basic/skip/skip.wacc")).get.isSuccess) 
  }

  test("Valid Expressions Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/andExpr.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/andOverOrExpr.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/boolCalc.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/boolExpr1.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/charComparisonExpr.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/divExpr.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/equalsExpr.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/equalsOverAnd.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/equalsOverBool.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/equalsOverOr.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/greaterEqExpr.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/greaterExpr.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/intCalc.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/intExpr1.wacc")).get.isSuccess) 
    assert(newParser.parseFromFile(new File("wacc_examples/valid/expressions/lessCharExpr.wacc")).get.isSuccess) 
  }

  test("Valid Functions Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/fibonacciFullRec.wacc")).get.isSuccess)     
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/fibonacciRecursive.wacc")).get.isSuccess)  
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/fixedPointRealArithmetic.wacc")).get.isSuccess)  
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/functionConditionalReturn.wacc")).get.isSuccess)  
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/mutualRecursion.wacc")).get.isSuccess)  
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/printInputTriangle.wacc")).get.isSuccess)  
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/printTriangle.wacc")).get.isSuccess)  
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/nested_functions/simpleRecursion.wacc")).get.isSuccess)  

    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/asciiTable.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionDeclaration.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionDoubleReturn.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionIfReturns.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionManyArguments.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionMultiReturns.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionReturnPair.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionSimple.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionSimpleLoop.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/functionUpdateParameter.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/incFunction.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/negFunction.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/sameArgName.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/sameArgName2.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/function/simple_functions/sameNameAsVar.wacc")).get.isSuccess)
  }

  test("Valid Conditional Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if1.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if2.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if3.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if4.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if5.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/if6.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/ifBasic.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/ifFalse.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/ifTrue.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/if/whitespace.wacc")).get.isSuccess) 
  }

  test("Valid IO Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/hashInProgram.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/multipleStringsAssignment.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/print-backspace.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/print-carridge-return.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/print.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printBool.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printChar.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printCharArray.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printCharAsString.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printEscChar.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/printInt.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/print/println.wacc")).get.isSuccess)

    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoBigInt.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoBigNegInt.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoChar.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoInt.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoNegInt.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/echoPuncChar.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/IO/read/read.wacc")).get.isSuccess)
  }

  test("Valid Pairs Test") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/checkRefPair.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/createPair.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/createPair02.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/createPair03.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/createRefPair.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/free.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/linkedList.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/nestedPair.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/null.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/printNull.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/printNullPair.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/printPair.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/printPairOfNulls.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/readPair.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/writeFst.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/pairs/writeSnd.wacc")).get.isSuccess)
  }
  

  test("Valid Runtime Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/arrayOutOfBounds/arrayNegBounds.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBounds.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/arrayOutOfBounds/arrayOutOfBoundsWrite.wacc")).get.isSuccess)

    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/divideByZero/divideByZero.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/divideByZero/divZero.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/divideByZero/modByZero.wacc")).get.isSuccess)

    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intJustOverflow.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intmultOverflow.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intnegateOverflow.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intnegateOverflow2.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intnegateOverflow3.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intnegateOverflow4.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intUnderflow.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/integerOverflow/intWayOverflow.wacc")).get.isSuccess)

    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/freeNull.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/readNull1.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/readNull2.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/setNull1.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/setNull2.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/useNull1.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/runtimeErr/nullDereference/useNull2.wacc")).get.isSuccess)
  }

  test("Valid Scope Tests"){
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/ifNested1.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/ifNested2.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/indentationNotImportant.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/intsAndKeywords.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/printAllTypes.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/scope.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/scopeBasic.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/scopeIfRedefine.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/scopeRedefine.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/scopeSimpleRedefine.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/scopeVars.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/scopeWhileNested.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/scope/scopeWhileRedefine.wacc")).get.isSuccess)
  }

  test("Valid Sequence Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/basicSeq.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/basicSeq2.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/boolAssignment.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/charAssignment.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/exitSimple.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/intAssignment.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/intLeadingZeros.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/sequence/stringAssignment.wacc")).get.isSuccess)
  }

  test("Valid Variables Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/_VarNames.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/boolDeclaration.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/boolDeclaration2.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/capCharDeclaration.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/charDeclaration.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/charDeclaration2.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/emptyStringDeclaration.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/intDeclaration.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/longVarNames.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/manyVariables.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/negIntDeclaration.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/puncCharDeclaration.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/stringDeclaration.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/variables/zeroIntDeclaration.wacc")).get.isSuccess)
  }

  test("Valid While Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/fibonacciFullIt.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/fibonacciIterative.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/loopCharCondition.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/loopIntCondition.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/max.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/min.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/rmStyleAdd.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/rmStyleAddIO.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/whileBasic.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/whileBoolFlip.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/whileCount.wacc")).get.isSuccess)
    assert(newParser.parseFromFile(new File("wacc_examples/valid/while/whileFalse.wacc")).get.isSuccess)
  }

  test("Invalid Array Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/array/arrayExpr.wacc")).get.isFailure)
  }

  test("Invalid Basic Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/basic/badComment.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/basic/badComment2.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/basic/badEscape.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/basic/bgnErr.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/basic/multipleBegins.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/basic/noBody.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/basic/skpErr.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/basic/unescapedChar.wacc")).get.isFailure)
  }

  test("Invalid Expressions Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/expressions/missingOperand1.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/expressions/missingOperand2.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/expressions/printlnConcat.wacc")).get.isFailure)
  }

   test("Invalid Function Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/badlyNamed.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/badlyPlaced.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/funcExpr.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/funcExpr2.wacc")).get.isFailure)
    // assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/functionConditionalNoReturn.wacc")).get.isFailure)
    // assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/FunctionEndingNotReturn.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/functionLateDefine.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/functionMissingCall.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/functionMissingParam.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/functionMissingPType.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/functionMissingType.wacc")).get.isFailure)
    // assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/functionNoReturn.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/functionScopeDef.wacc")).get.isFailure)
    // assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/mutualRecursionNoReturn.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/noBodyAfterFuncs.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/function/thisIsNotC.wacc")).get.isFailure)
  }

  test("Invalid If Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/if/ifiErr.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/if/ifNoelse.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/if/ifNofi.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/if/ifNothen.wacc")).get.isFailure)
  }

  test("Invalid Pairs Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/pairs/badLookup01.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/pairs/badLookup02.wacc")).get.isFailure)
  }

  test("Invalid Print Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/print/printlnCharArry.wacc")).get.isFailure)
  }

  test("Invalid Sequence Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/sequence/doubleSeq.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/sequence/emptySeq.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/sequence/endSeq.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/sequence/extraSeq.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/sequence/missingSeq.wacc")).get.isFailure)
  }

  test("Invalid Variables Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/variables/badintAssignments.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/variables/badintAssignments1.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/variables/badintAssignments2.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/variables/bigIntAssignment.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/variables/varNoName.wacc")).get.isFailure)
  }

  test("Invalid While Tests") {
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/while/donoErr.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/while/dooErr.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/while/whileNodo.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/while/whileNodone.wacc")).get.isFailure)
    assert(newParser.parseFromFile(new File("wacc_examples/invalid/syntaxErr/while/whilErr.wacc")).get.isFailure)
  }
}
