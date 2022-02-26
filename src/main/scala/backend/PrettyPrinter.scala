package backend

import backend.Opcodes._
import java.io.{File, FileWriter}

object PrettyPrinter {
  private val tab = "\t"
  private val doubleTab = tab * 2

  def prettyPrint(
      waccFilename: String,
      instrs: List[(Label, List[Instr])]
  ): Unit = {
    val assemblyFilename = waccFilename.replaceFirst(".wacc", ".s")
    var file = new File(assemblyFilename)

    file.delete()
    file.createNewFile()
    
    val fileWriter = new FileWriter(file)

    fileWriter.write(tab + ".text\n\n" + tab + ".global main\n")

    instrs.foreach((x: (Label, List[Instr])) => {
        fileWriter.write(tab + x._1 + ":\n")
        x._2.foreach((i: Instr) => fileWriter.write(doubleTab + i + "\n"))
    }
    )
    fileWriter.close()
  }
}
