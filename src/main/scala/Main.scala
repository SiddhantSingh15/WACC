import java.io.File

import frontend._
import parsley.Parsley, Parsley._
import SyntaxErrors.{TestError}
import parsley.Success
import parsley.Failure
import backend._

object Main {

  def main(args: Array[String]): Unit =  {
    val file = new File(args(0))

    assert(file.exists())
    // println("Parsing: " + args(0))

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

        // println(Console.GREEN + s"${args(0)} is synctactically valid.")
        if (!semRes.isEmpty) {
            for (error <- semRes.toList) {
            println("[" + Console.RED + "error" + Console.RESET + "]: " + error)
            System.exit(EXITCODE_SEM_ERROR)
          }
        }

        // println(Console.GREEN + s"${args(0)} is semantically valid.")
          // System.exit(EXITCODE_SUCC)
          val programTree = parsed.get
          val codeGen = backend.CodeGen
          val (data, instructions) = codeGen.transProgram(programTree, symbTable)

          val prettyPrinter = backend.PrettyPrinter
          prettyPrinter.prettyPrint(file.getName(), data, instructions)

      case Failure(err) =>
        print(err.getError())
        System.exit(EXITCODE_SYNTAX_ERROR)
      }

    }

}
