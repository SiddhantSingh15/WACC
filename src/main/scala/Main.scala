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
          val semRes = semChecker.checkProgram(parsed.get)._2
          for (err <- semRes) {
            if (err.isInstanceOf[FuncNoRetErr]) {
              println("[error]: " + err)
              System.exit(100)
            }
          }

          println(s"${args(0)} is synctactically valid.")
          if (semRes.isEmpty) {
            println(s"${args(0)} is semantically valid.")
            System.exit(0)
          }
          for (error <- semRes.toList) {
            println("[error]: " + error)
          }
          System.exit(200)
        case Failure(err) =>
          print(err.getError())
          System.exit(100)
        }
    }
}
