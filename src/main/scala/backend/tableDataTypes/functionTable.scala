package backend.tableDataTypes

import backend.Opcodes._
import scala.collection.mutable.ListBuffer

class functionTable {

  private var labelCounter = 0
  val table = ListBuffer.empty[(Label, List[Instr])]

  def add(label: Label, instrs: ListBuffer[Instr]): Unit = {
    table += ((label, instrs.toList))
  }

  def contains(label: Label): Boolean = {
    for ((l, _) <- table) {
      if (label == l) {
        return true
      }
    }
    return false
  }
  
}
