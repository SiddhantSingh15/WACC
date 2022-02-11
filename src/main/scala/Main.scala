import java.io.File

import frontend._

object Main {
  val parser = frontend.Parser
  val semChecker = frontend.SemanticChecker

  def main(args: Array[String]) =  {
    val parsed = parser.parseFromFile(new File(args(0))).get.get
    val semRes = semChecker.checkProgram(parsed)._2
    for (error <- semRes.toList) {
      println("[error]: " + error)
    }
  }
}
