import scala.collection.mutable.ListBuffer
import java.io.File
import scala.io.Source
import scala.util.matching.Regex
import scala.sys.process._

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

  def createDummyOutput(file: File): (File, File, ProcessBuilder) = {
    makeExec(file)
    val fName = file.getName().replaceAll(".wacc", "")
    val result = new File(s"$fName.out")
    val inputs = Source.fromFile(getFileFromPath(file, "wacc_examples/expected")).getLines().mkString("\n")
    val cmd = 
    if (inputs.isEmpty) {
      s"qemu-arm -L /usr/arm-linux-gnueabi/ $fName" #> result
    } else {
      (s"printf '$inputs'" #| s"qemu-arm -L /usr/arm-linux/gnueabi/ $fName" #> result)
    }

    (file, result, cmd)
  }

  private def getFileFromPath(file: File, folder: String): File = {
    new File (file.getPath().replace("wacc", "txt").replace("wacc_examples/valid", folder))
  }

  private def makeExec(file: File): Unit = {
    val name = file.getPath()
    s"java -jar target/scala-2.13/wacc.jar $name".!
    val changeName = file.getName().replaceAll(".wacc", "")
    s"arm-linux-gnueabi-gcc -o $name -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $name.s".!
    val exec = new File(file.getName.replaceAll(".wacc", ".s"))
    exec.delete()
  }

  def checkOutput(file: File, result: File): Boolean = {
    val expected = getFile(file, "wacc_examples/expected")
    var printed = Source.fromFile(result).getLines().mkString("\n")
    val expPrinted = Source.fromFile(expected).getLines().mkString("\n")

    result.delete

    val registerAddr = "0x[0-9a-z]+".r
    printed = registerAddr replaceAllIn (printed, "#addr#")
    expPrinted.equals(printed)
  }

  private def getFile(file: File, folder: String): File = {
    new File(file.getPath().replace("wacc_examples/valid", folder).replace(".wacc", ".txt"))    
  }

}
