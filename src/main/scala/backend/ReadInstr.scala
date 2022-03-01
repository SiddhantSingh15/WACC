package backend

import Opcodes._
import Operand._
import scala.collection.mutable.ListBuffer

object ReadInstr {

  final val resultReg: Register = R0

  def charRead(label: Label): (Label, ListBuffer[Instr]) = {
    (
      Label("p_read_char"),
      ListBuffer(
        Push(ListBuffer(R14_LR)),
        Mov(R1, resultReg),
        Ldr(resultReg, DataLabel(label)),
        Add(resultReg, resultReg, Imm_Int(4)),
        Bl(Label("scanf")),
        Pop(ListBuffer(R15_PC))
      )
    )
  }

  def intRead(label: Label): (Label, ListBuffer[Instr]) = {
    (
      Label("p_read_int"),
      ListBuffer(
        Push(ListBuffer(R14_LR)),
        Mov(R1, resultReg),
        Ldr(resultReg, DataLabel(label)),
        Add(resultReg, resultReg, Imm_Int(4)),
        Bl(Label("scanf")),
        Pop(ListBuffer(R15_PC))
      )
    )
  }
}