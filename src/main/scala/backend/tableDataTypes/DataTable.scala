package backend.tableDataTypes

import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import frontend.AST.StrLiter

class DataTable {
  var table = ListBuffer.empty[Data]
  var size = 0

  /*
   * Adds STRLITERAL to DataTable, returns a label.
   */
  def addStrLiter(strliter: StrLiter): Label = {
    addData(strliter.toString())
  }

  /*
   * Adds a string to the data table.
   */
  def addData(str: String): Label = {
    val label = Label(getNext())
    table += Data(label, str)
    label
  }

  /*
   * Helper function to get the next message.
   */
  private def getNext(): String ={
    val label = "msg_" + size.toString
    size += 1
    label
  }

  /*
   * Adds a label to the data table. Returns the label.
   */
  def addLabel(label: String, str: String): Label = {
    val l = Label(label)
    if (!contains(l)) {
      table += Data(l, str)
    }
    l
  }

  /*
   * Helper functions to addLabel().
   * Returns true if the table contains the label. 
   */
  private def contains(label: Label): Boolean = {
    return !table.filter({case Data(l, _) => l == label}).isEmpty
  }
}
