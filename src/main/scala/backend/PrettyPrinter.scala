package backend

import backend.Opcodes._
import java.io.{File, FileWriter}
import scala.collection.mutable.StringBuilder

object PrettyPrinter {

  def prettyPrint(waccFilename: String, data: List[Data], instrs: List[(Label, BlockInstrs)]): Unit = {
    val assemblyFilename = waccFilename.replaceFirst(".wacc", ".s")
    var file = new File(assemblyFilename)

    file.delete()
    file.createNewFile()
    
    val fileWriter = new FileWriter(file)

    if (!data.isEmpty) {
      fileWriter.write("\t.data\n\n")
      data.foreach(d => printDataTable(d, fileWriter))
      fileWriter.write("\n")
    }

    fileWriter.write("\t.text\n\n\t.global main\n")

    instrs.foreach((x: (Label, BlockInstrs)) => {
        val labelStr = x._1
        fileWriter.write(s"\t$labelStr:\n")
        fileWriter.write(x._2.toString())
    })
    fileWriter.close()
  }

  private def printDataTable(d: Data, fileWriter: FileWriter): Unit = {
    val Data(Label(l), s) = d
    fileWriter.write(s"\t$l:\n")
    val len = size(s)
    fileWriter.write(s"\t\t.word $len\n")
    fileWriter.write(s"\t\t.ascii \"$s\"\n")
  }

  private def size(str: String): Int = {
    val chars = """\\[0btnfr"'\\]"""
    str.length - chars.r.findAllIn(str).length
  }
}
