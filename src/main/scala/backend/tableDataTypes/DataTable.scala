package backend.tableDataTypes

import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import frontend.AST.StrLiter

class dataTable {
  var table = ListBuffer.empty[Data]
  var size = 0

  def addData(str: StrLiter): Label = {
    val label = Label(getNext())
    table += Data(label, str.toString())
    label
  }

  private def getNext(): String ={
    val label = "data:" + size.toString
    size += 1
    label
  }

  def addLabel(label: String, str: String): Label = {
    val l = Label(label)
    if (!contains(l)) {
      table += Data(l, str)
    }
    l
  }

  private def contains(label: Label): Boolean = {
    return !table.filter({case Data(l, _) => l == label}).isEmpty
  }
}
