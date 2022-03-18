import java.io.File

import frontend._
import parsley.Parsley, Parsley._
import SyntaxErrors.{TestError}
import parsley.Success
import parsley.Failure
import backend._
import backend.CodeGen._

object Main {

  // The first argument is the filename.
  // The second argument is the number catering to the optimisation technique
  //    1 - Constant Evaluation
  //    2 - Constant Propagation
  //    3 - Control Flow Analysis
  //    4 - Peephole Optimisation

  def main(args: Array[String]): Unit =  {
    val file = new File(args(0))

    assert(file.exists())

    if (args.size == 2) {
      val optiTech = args(1).toInt
      optiTech match {
        case 1 => 
          constantEvaluation = true
        case 2 =>
          constantEvaluation = true
          constantPropagation = true
        case 3 =>
          constantEvaluation = true
          constantPropagation = true
          controlFA = true
        case 4 =>
          constantEvaluation = true
          constantPropagation = true
          controlFA = true
          peephole = true
      }
    }

    val EXITCODE_SUCC = 0
    val EXITCODE_SYNTAX_ERROR = 100
    val EXITCODE_SEM_ERROR = 200

    val parser = frontend.Parser
    val parsed = parser.parseFromFile(file).get

    val parsedResult = parsed match {
      case Success(x) =>
        val semChecker = frontend.SemanticChecker
        val (symbTable, semRes) = semChecker.checkProgram(parsed.get)
        for (err <- semRes) {
          if (err.isInstanceOf[FuncNoRetErr]) {
            println("[" + Console.RED + "error" + Console.RESET+ "]: " + err)
            System.exit(EXITCODE_SYNTAX_ERROR)
          }
        }

        if (!semRes.isEmpty) {
            for (error <- semRes.toList) {
            println("[" + Console.RED + "error" + Console.RESET + "]: " + error)
            System.exit(EXITCODE_SEM_ERROR)
          }
        }

        val programTree = parsed.get
        var (data, instructions) = transProgram(programTree, symbTable)

        val prettyPrinter = backend.PrettyPrinter
        
        if (peephole) {
          instructions = InstrEval.optimiseBlocks(instructions)
        }
        prettyPrinter.prettyPrint(file.getName(), data, instructions)

      case Failure(err) =>
        print(err.getError())
        System.exit(EXITCODE_SYNTAX_ERROR)
    }
  }
}
