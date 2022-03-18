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

    // Checking that the file exists
    assert(file.exists())

    // Setting the flags for the optimisation technique
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

    // Generates the AST from the WACC Program
    val parsed = frontend.Parser.parseFromFile(file).get

    parsed match {

      // No syntax error detected during parsing
      case Success(x) =>
        val programTree = parsed.get
        // Checks the AST for semantic errors 
        val (symbTable, semRes) = frontend.SemanticChecker
            .checkProgram(programTree)

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

        // Generates instructions based on ARM11 Internal Representation
        // using the AST
        var (data, instructions) = transProgram(programTree, symbTable)

        // Optimises the instructions using peephole optimisation
        if (peephole) {
          instructions = InstrEval.optimiseBlocks(instructions)
        }

        // Creates the ARM11 Assembly file based on the generated
        // block of instructions
        backend.PrettyPrinter.prettyPrint(file.getName(), data, instructions)

      // Syntax error detected during parsing
      case Failure(err) =>
        print(err.getError())
        System.exit(EXITCODE_SYNTAX_ERROR)
    }
  }
}
