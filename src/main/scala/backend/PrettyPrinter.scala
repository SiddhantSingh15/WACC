package backend

import backend.Opcodes._
import java.io.{File, FileWriter}

object PrettyPrinter {
  private val tab = "\t"
  private val doubleTab = tab * 2

  def prettyPrint(
      waccFilename: String,
      data: List[Data],
      instrs: List[(Label, List[Instr])]
  ): Unit = {
    val assemblyFilename = waccFilename.replaceFirst(".wacc", ".s")
    var file = new File(assemblyFilename)

    file.delete()
    file.createNewFile()
    
    val fileWriter = new FileWriter(file)

    if (!data.isEmpty) {
      fileWriter.write(tab + ".data\n\n")
      data.foreach(d => printDataTable(d, fileWriter))
      fileWriter.write("\n")
    }

    fileWriter.write(tab + ".text\n\n" + tab + ".global main\n")

    instrs.foreach((x: (Label, List[Instr])) => {
        fileWriter.write(tab + x._1 + ":\n")
        x._2.foreach((i: Instr) => fileWriter.write(doubleTab + i + "\n"))
    }
    )
    fileWriter.close()
  }

  private def printDataTable(d: Data, fW: FileWriter): Unit = {
    val Data(Label(l), s) = d
    fW.write(tab + l)
    fW.write(":\n")
    fW.write(doubleTab + ".word " + size(s))
    fW.write("\n")
    fW.write(doubleTab + ".ascii \"")
    fW.write(s + "\"" + "\n")
  }

  private def size(str: String): Int = {
    val chars = """"\\[0btnfr"'\\]""""
    str.length - chars.r.findAllIn(str).length
  }
}
