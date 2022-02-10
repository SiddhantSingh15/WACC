import java.io.File

import frontend._

object Main {
  val parser = frontend.Parser
  val semChecker = frontend.SemanticChecker

  def main(args: Array[String]) =  {
    val parsed = parser.parseFromFile(new File("wacc_examples/invalid/semanticErr/exit/exitNonInt.wacc")).get.get
    val (symbTable, semRes) = semChecker.checkProgram(parsed)
    print(semRes)
  }
}
