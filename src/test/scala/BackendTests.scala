import org.scalatest.funsuite.AnyFunSuite
import CodeGenTestSuite._
import scala.collection.immutable.List
import java.io.File

class BackendTests extends AnyFunSuite {
  
  val dir = new File("wacc_examples/expected/")
  val listOfDir = dir.listFiles.map(_.getName).toList

  for (subDir <- listOfDir) {
    for (file <- loadFile(s"wacc_examples/valid/$subDir")) {
      val fName = file.getName
      println(s"Testing $fName")
      val (f, output, cmd) = createDummyOutput(file)
      test(s"Expected Test Name: ($fName)") {
        assert(checkOutput(f, output))
      }
    }
  }
}


