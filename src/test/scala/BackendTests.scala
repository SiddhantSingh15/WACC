import org.scalatest.funsuite.AnyFunSuite
import CodeGenTestSuite._
import scala.collection.immutable.List
import java.io.File

class BackendTests extends AnyFunSuite {
  
  val dir = new File("wacc_examples/valid/")
  val listOfDir = dir.listFiles.map(_.getName).toList

  val listOfOpti = List("Unoptimised",
                        "Constant Evaluation",
                        "Constant Propagation",
                        "Control Flow Analysis",
                        "Peephole Optimisation")
  val toAvoidForPeephole = List("divideByZero.wacc",
                                "divideByZero2.wacc",
                                "divZero.wacc",
                                "divZero2.wacc")

  for ((opti, i) <- listOfOpti.zipWithIndex) {                  
    for (subDir <- listOfDir) {
      if (!subDir.equals("advanced")) {
        for (f <- getFilesFrom(s"wacc_examples/valid/$subDir")) {
          val name = f.getName
          if (!(i == 4 && toAvoidForPeephole.contains(name))) {
            val (file, out, command) = createOutputFiles(f, i)
            test(s"$opti exit code test for $name") {
              assert(checkExitCode(file, command))
            }
            test(s"$opti expected test for $name") {
              assert(checkStdOut(file, out))
            }
          }
        }
      }
    }
  }
}


