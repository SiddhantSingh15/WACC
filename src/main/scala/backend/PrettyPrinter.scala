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
    
    val fw = new FileWriter(file)

    if (!data.isEmpty) {
      fw.write("\t.data\n\n")
      data.foreach(d => printDataTable(d, fw))
      fw.write("\n")
    }

    fw.write("\t.text\n\n\t.global main\n")

    instrs.foreach((x: (Label, BlockInstrs)) => {
        val labelStr = x._1
        fw.write(s"\t$labelStr:\n")
        fw.write(x._2.toString())
    })
    fw.close()
  }

  private def printDataTable(d: Data, fw: FileWriter): Unit = {
    val Data(Label(l), s) = d
    fw.write(s"\t$l:\n")
    val len = size(s)
    fw.write(s"\t\t.word $len\n")
    fw.write(s"\t\t.ascii \"$s\"\n")
  }

  private def size(str: String): Int = {
    val chars = """\\[0btnfr"'\\]"""
    str.length - chars.r.findAllIn(str).length
  }
}
