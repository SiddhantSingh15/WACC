import scala.collection.mutable.ListBuffer
import java.io.File
import scala.io.Source
object CodeGenTestSuite {

  def loadFile(path: String): ListBuffer[File] = {
    var result = ListBuffer.empty[File]
    val folder = new File(path)
    for (file <- folder.listFiles) {
      if (file.isFile) {
        result += file
      } else {
        result ++= loadFile(file.getPath)
      }
    }

    result
  }

  def checkOutput(file: File, result: File): Boolean = {
    val expected = getExpected(file)
    var printed = readLines(result)
    val expPrinted = readLines(expected)

    result.delete

    val registerAddr = "0x[0-9a-z]+".r

    printed = registerAddr replaceAllIn (printed, "#addr#")

    expPrinted.equals(printed)
  }
  
  private def readLines(file: File): String = {
    Source.fromFile(file).getLines().mkString("\n")
  }

  private def getExpected(file: File): File = {
    getFile(file, "expected")
  }

  private def getFile(file: File, folder: String): File = {
    new File(file.getPath().replace("wacc_examples/valid", folder).replace(".wacc", ".txt"))    
  }

}
