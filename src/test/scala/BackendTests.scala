import org.scalatest.funsuite.AnyFunSuite
import CodeGenTestSuite._

class BackendTests extends AnyFunSuite {
  test ("Expected Basic Tests") {
    for (file <- loadFile("wacc_example/valid/basic/")) {
      val fName = file.getName
      val (f, output, cmd) = createDummyOutput(file)
      test(s"Expected Test Name: ($fName)") {
        assert(checkOutput(f, output))
      }
    }
  }
}
