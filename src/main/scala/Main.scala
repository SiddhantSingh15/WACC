import java.io.File

import frontend._
import parsley.Parsley, Parsley._
import WACCErrors.{TestError}
import parsley.Success
import parsley.Failure

object Main {
  val parser = frontend.Parser
  val semChecker = frontend.SemanticChecker

  def main(args: Array[String]): Unit =  {
    assert(args.length == 1)
    println("Parsing: " + args(0))

    val parsed = parser.parseFromFile(new File(args(0))).get

    val parsedResult = parsed match {
        case Success(x) =>
          println(x)
          println(s"${args(0)} is synctactically valid.")
          val semRes = semChecker.checkProgram(parsed.get)._2
          if (semRes.toList.length == 0) {
            println(s"${args(0)} is semantically valid.")
            // System.exit(0)
          }
          for (error <- semRes.toList) {
            println("[error]: " + error + "\n")
          }
          // System.exit(100)
        case Failure(err) =>
          println("reach here")
          print(err.getError())
          // System.exit(200)
        }
    }
}
