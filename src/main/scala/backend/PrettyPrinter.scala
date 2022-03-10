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

    val sb = new StringBuilder

    if (!data.isEmpty) {
      sb ++= ".data\n\n"
      data.foreach(d => printDataTable(d, sb))
      sb ++= "\n"
    }

    sb ++= "\t.text\n\n\t.global main\n"

    instrs.foreach((x: (Label, BlockInstrs)) => {
        val labelStr = x._1
        sb ++= s"\t$labelStr:\n"
        sb ++= x._2.toString()
    })
    fileWriter.write(sb.toString())
    fileWriter.close()
  }

  private def printDataTable(d: Data, sb: StringBuilder): Unit = {
    val Data(Label(l), s) = d
    sb ++= s"\t$l:\n"
    val len = size(s)
    sb ++= s"\t\t.word $len\n"
    sb ++= s"\t\t.ascii \"$s\"\n"
  }

  private def size(str: String): Int = {
    val chars = """\\[0btnfr"'\\]"""
    str.length - chars.r.findAllIn(str).length
  }
}
