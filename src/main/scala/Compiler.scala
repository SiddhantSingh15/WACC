import java.io.File
object Compiler {

  val parser = frontend.Parser
  val semChecker = frontend.SemanticChecker

  def check: Unit = {
    val parsed = parser.parseFromFile(new File("wacc_examples/invalid/semanticErr/exit/badCharExit.wacc")).get.get
    val (symbTable, semRes) = semChecker.checkProgram(parsed)
    print(semRes)
  }
}