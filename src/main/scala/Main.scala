import java.io.File

import frontend._

object Main {
  val parser = frontend.Parser
  val semChecker = frontend.SemanticChecker

  def main(args: Array[String]) =  {
    val parsed = parser.parseFromFile(new File("wacc_examples/invalid/semanticErr/expressions/boolOpTypeErr.wacc")).get.get
    val semRes = semChecker.checkProgram(parsed)
    println(semRes.toList.toString())
  }
}
