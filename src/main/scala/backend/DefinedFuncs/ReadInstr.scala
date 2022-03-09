package backend

import Opcodes._
import Operand._
import scala.collection.mutable.ListBuffer
import CodeGen.resultRegister

object ReadInstr {

  def charRead(label: Label): (Label, BlockInstrs) = {
    (
      Label("p_read_char"),
      BlockInstrs(ListBuffer(
        Push(ListBuffer(R14_LR)),
        Mov(R1, resultRegister),
        Ldr(resultRegister, DataLabel(label)),
        Add(resultRegister, resultRegister, Imm_Int(4)),
        Bl(Label("scanf")),
        Pop(ListBuffer(R15_PC))
      ))
    )
  }

  def intRead(label: Label): (Label, BlockInstrs) = {
    (
      Label("p_read_int"),
      BlockInstrs(ListBuffer(
        Push(ListBuffer(R14_LR)),
        Mov(R1, resultRegister),
        Ldr(resultRegister, DataLabel(label)),
        Add(resultRegister, resultRegister, Imm_Int(4)),
        Bl(Label("scanf")),
        Pop(ListBuffer(R15_PC))
      ))
    )
  }
}