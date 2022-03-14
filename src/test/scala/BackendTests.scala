import org.scalatest.funsuite.AnyFunSuite
import CodeGenTestSuite._
import scala.collection.immutable.List
import java.io.File

class BackendTests extends AnyFunSuite {
  
  val dir = new File("wacc_examples/valid/")
  val listOfDir = dir.listFiles.map(_.getName).toList

  // for (subDir <- listOfDir) {
    val subDir = "IO/print"
    if (!subDir.equals("advanced") && !subDir.equals("runtimeErr")) {
      for (f <- getFilesFrom(s"wacc_examples/valid/$subDir")) {
        val name = f.getName
        val (file, out, command) = createOutputFiles(f)
        test(s"Array exit code test for $name") {
          assert(checkExitCode(file, command))
        }
        test(s"Array expected test for $name") {
          assert(checkStdOut(file, out))
        }
      }
    // }
  }
}


