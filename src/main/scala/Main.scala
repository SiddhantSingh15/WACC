import java.io.File

import frontend._
import parsley.Parsley, Parsley._
import SyntaxErrors.{TestError}
import parsley.Success
import parsley.Failure
import backend._

object Main {
  val parser = frontend.Parser
  val semChecker = frontend.SemanticChecker
  val codeGen = backend.CodeGen;
  val prettyPrinter = backend.PrettyPrinter;

  def main(args: Array[String]): Unit =  {
    assert(args.length == 1)
    println("Parsing: " + args(0))

    val EXITCODE_SUCC = 0
    val EXITCODE_SYNTAX_ERROR = 100
    val EXITCODE_SEM_ERROR = 200

    val file = new File(args(0))
    val parsed = parser.parseFromFile(file).get

    val parsedResult = parsed match {
      case Success(x) =>
        // println(x)
        val semRes = semChecker.checkProgram(parsed.get)._2
        for (err <- semRes) {
          if (err.isInstanceOf[FuncNoRetErr]) {
            println("[" + Console.RED + "error" + Console.RESET+ "]: " + err)
            System.exit(EXITCODE_SYNTAX_ERROR)
          }
        }

        println(Console.GREEN + s"${args(0)} is synctactically valid.")
        if (semRes.isEmpty) {
          println(Console.GREEN + s"${args(0)} is semantically valid.")
          // System.exit(EXITCODE_SUCC)
          val programTree = parsed.get
          val (data, instructions) = codeGen.transProgram(programTree)
          prettyPrinter.prettyPrint(file.getName(), data, instructions)
        }

        for (error <- semRes.toList) {
          println("[" + Console.RED + "error" + Console.RESET + "]: " + error)
        }

        System.exit(EXITCODE_SEM_ERROR)
      case Failure(err) =>
        print(err.getError())
        System.exit(EXITCODE_SYNTAX_ERROR)
      }

    }

}
