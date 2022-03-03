package backend.tableDataTypes

import backend.Opcodes._
import scala.collection.mutable.ListBuffer

class functionTable {

  private var counter = 0
  val table = ListBuffer.empty[(Label, List[Instr])]

  /*
   * Adds function to funcTable.
   */
  def addFunction(label : Label, instrs : ListBuffer[Instr]) : Unit = {
    table += ((label, instrs.toList))
  }

  /*
   * Adds functions as a pair with (Label, list of instructions)
   */
  def addFunction(tup : (Label, ListBuffer[Instr])) : Unit = {
    val (label, instrs) = tup 
    if(!contains(label)){
      val entry = (label, instrs.toList)
      table += entry 
    }
  }

  /*
   * Adds to funcTable
   */
  def add(label: Label, instrs: ListBuffer[Instr]): Unit = {
    table += ((label, instrs.toList))
  }

  /*
   * Helper function for getNext().
   * Returns true if funcTable contains passed in Label.
   */
  private def contains(label: Label): Boolean = {
    return !table.filter({case (l, _) => l == label}).isEmpty 
  }

  /*
   * Gets the next label from funcTable.
   */
  def getNext() : Label = {
    val nextLabel = Label("L" + counter)
    counter += 1
    nextLabel
  }
}
