package backend

import Opcodes._
import Operand._
import scala.collection.mutable.ListBuffer

object ReadInstructions {

  final val resultReg: Register = R0

  def charRead(label: Label): (Label, List[Instr]) = {
    (
      Label("p_read_char"),
      List[Instr](
        Push(ListBuffer(R14_LR)),
        Mov(R1, resultReg),
        Ldr(resultReg, DataLabel(label)),
        Add(resultReg, resultReg, Imm_Int(4)),
        Bl(Label("scanf")),
        Pop(ListBuffer(R15_PC))
      )
    )
  }

  def intRead(label: Label): (Label, List[Instr]) = {
    (
      Label("p_read_int"),
      List[Instr](
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