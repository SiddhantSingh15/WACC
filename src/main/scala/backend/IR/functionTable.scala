package backend.tableDataTypes

import backend.Opcodes._
import scala.collection.mutable.ListBuffer

class FunctionTable {

  private var counter = 0
  val table = ListBuffer.empty[(Label, BlockInstrs)]

  /*
   * Adds function to funcTable.
   */
  def add(label : Label, instrs : BlockInstrs) : Unit = {

    if (!contains(label)) {
      table += ((label, instrs))
    }
  }

  /*
   * Adds functions as a pair with (Label, list of instructions)
   */
  def addFunction(tup : (Label, BlockInstrs)) : Unit = {
    val (label, instrs) = tup 
    if(!contains(label)){
      val entry = (label, instrs)
      table += entry 
    }
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
