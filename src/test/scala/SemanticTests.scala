import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Paths
import java.io.File
import parsley.Success
import parsley.Failure
import frontend._

class SemanticTests extends AnyFunSuite {
  val newParser = frontend.Parser;

  test("Function has return Tests") {
    assert(helperFunc(new File("wacc_examples/invalid/syntaxErr/function/functionConditionalNoReturn.wacc")))
    assert(helperFunc(new File("wacc_examples/invalid/syntaxErr/function/functionEndingNotReturn.wacc")))
    assert(helperFunc(new File("wacc_examples/invalid/syntaxErr/function/functionNoReturn.wacc")))
    assert(helperFunc(new File("wacc_examples/invalid/syntaxErr/function/mutualRecursionNoReturn.wacc")))
  }

  def helperFunc(file: File): Boolean = {
    val parser = frontend.Parser
    val semChecker = frontend.SemanticChecker
    val parsed = parser.parseFromFile(file).get

    val parsedResult = parsed match {
        case Success(x) =>
          val semRes = semChecker.checkProgram(parsed.get)._2
          // print(semRes)
          for (err <- semRes) {
            if (err.isInstanceOf[FuncNoRetErr]) {
              return true
            }
          }  
          false      
        case Failure(err) =>
          false
        }
    
    parsedResult
    }

  test("Invalid Semantic Errors Exit Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/exit/badCharExit.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/exit/exitNonInt.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/exit/globalReturn.wacc")))
  }

  test("Invalid Semantic Errors Expressions Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/expressions/boolOpTypeErr.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/expressions/exprTypeErr.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/expressions/intOpTypeErr.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/expressions/lessPairExpr.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/expressions/mixedOpTypeErr.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/expressions/moreArrExpr.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/expressions/stringElemErr.wacc")))
  }

  test("Invalid Semantic Errors Function Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/function/functionAssign.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/function/functionBadArgUse.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/function/functionBadCall.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/function/functionBadParam.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/function/functionBadReturn.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/function/functionOverArgs.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/function/functionRedefine.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/function/functionSwapArgs.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/function/functionUnderArgs.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/function/funcVarAccess.wacc")))
  }

  test("Invalid Semantic Errors If Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/if/ifIntCondition.wacc")))
  }

  test("Invalid Semantic Errors IO Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/IO/readTypeErr.wacc")))
  }

  test("Invalid Semantic Errors Multiple Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/multiple/funcMess.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/multiple/ifAndWhileErrs.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/multiple/messyExpr.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/multiple/multiCaseSensitivity.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/multiple/multiTypeErrs.wacc")))
  }

  test("Invalid Semantic Errors Pairs Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/pairs/freeNonPair.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/pairs/fstNull.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/pairs/sndNull.wacc")))
  }

  test("Invalid Semantic Errors Print Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/print/printTypeErr01.wacc")))
  }

  test("Invalid Semantic Errors Read Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/read/readTypeErr01.wacc")))
  }

  test("Invalid Semantic Errors Scope Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/scope/badScopeRedefine.wacc")))
  }

  test("Invalid Semantic Errors Variables Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr01.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr02.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr03.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr04.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr05.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr06.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr07.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr08.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr09.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr10.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr11.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/basicTypeErr12.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/caseMatters.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/doubleDeclare.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/undeclaredScopeVar.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/undeclaredVar.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/variables/undeclaredVarAccess.wacc")))
  }

  test("Invalid Semantic Errors While Tests") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/while/falsErr.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/while/truErr.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/while/whileIntCondition.wacc")))
  }

  test("Invalid Heap Errors") {
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/heap/badAddr.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/heap/badCalloc.wacc")))
    assert(helper2Func(new File("wacc_examples/invalid/semanticErr/heap/badDeref.wacc")))
  }

  def helper2Func(file: File): Boolean = {
    val parser = frontend.Parser
    val semChecker = frontend.SemanticChecker
    val parsed = parser.parseFromFile(file).get

    val parsedResult = parsed match {
        case Success(x) =>
          val semRes = semChecker.checkProgram(parsed.get)._2
          // print(semRes)
          if (!semRes.isEmpty) return true
          false 
        case Failure(err) =>
          false
        }
    
    parsedResult
  }  
  
}
