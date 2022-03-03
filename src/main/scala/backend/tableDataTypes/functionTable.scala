package backend.tableDataTypes

import backend.Opcodes._
import scala.collection.mutable.ListBuffer

class functionTable {

  private var counter = 0
  val table = ListBuffer.empty[(Label, List[Instr])]

  def addFunction(label : Label, instrs : ListBuffer[Instr]) : Unit = {
    table += ((label, instrs.toList))
  }

  def addFunction(tup : (Label, ListBuffer[Instr])) : Unit = {
    val (label, instrs) = tup 
    if(!contains(label)){
      val entry = (label, instrs.toList)
      table += entry 
    }
  }

  def add(label: Label, instrs: ListBuffer[Instr]): Unit = {
    table += ((label, instrs.toList))
  }

  private def contains(label: Label): Boolean = {
    return !table.filter({case (l, _) => l == label}).isEmpty 
  }

  def getNext() : Label = {
    val nextLabel = Label("L" + counter)
    counter += 1
    nextLabel
  }
  
}
