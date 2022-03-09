package backend.DefinedFuncs

import backend.CodeGen._
import backend.Condition.{EQ, NE}
import backend.Operand._
import PreDefinedFuncs._
import backend.Opcodes._
import backend.CodeGeneration.CodeGenHelper._

import scala.collection.mutable.ListBuffer

object PrintInstrs {
  private val RESET_INT = 0

  def stringPrintInstrs: (Label, BlockInstrs) = (
    PrintString.functionLabel,
    BlockInstrs(ListBuffer[Instr](
      Push(ListBuffer(R14_LR)),
      Ldr(R1, RegAdd(resultRegister)),
      Add(R2, resultRegister, Imm_Int(SIZE_ADDR)),
      Ldr(resultRegister, DataLabel(Label(PrintString.msgName(0)))),
      Add(resultRegister, resultRegister, Imm_Int(SIZE_ADDR)),
      Bl(Label("printf")),
      Mov(resultRegister, Imm_Int(RESET_INT)),
      Bl(Label("fflush")),
      Pop(ListBuffer(R15_PC))
    ))
  )

  def boolPrintInstrs: (Label, BlockInstrs) = (
    PrintBool.functionLabel,
    BlockInstrs(ListBuffer[Instr](
      Push(ListBuffer(R14_LR)),
      Cmp(resultRegister, Imm_Int(FALSE_INT)),
      LdrCond(NE, resultRegister, DataLabel(Label(PrintBool.msgName(0)))),
      LdrCond(EQ, resultRegister, DataLabel(Label(PrintBool.msgName(1)))),
      Add(resultRegister, resultRegister, Imm_Int(SIZE_ADDR)),
      Bl(Label("printf")),
      Mov(resultRegister, Imm_Int(RESET_INT)),
      Bl(Label("fflush")),
      Pop(ListBuffer(R15_PC))
    ))
  )

  def intPrintInstrs: (Label, BlockInstrs) = (
    PrintInt.functionLabel,
    BlockInstrs(ListBuffer[Instr](
      Push(ListBuffer(R14_LR)),
      Mov(R1, resultRegister),
      Ldr(resultRegister, DataLabel(Label(PrintInt.msgName(0)))),
      Add(resultRegister, resultRegister, Imm_Int(SIZE_ADDR)),
      Bl(Label("printf")),
      Mov(resultRegister, Imm_Int(RESET_INT)),
      Bl(Label("fflush")),
      Pop(ListBuffer(R15_PC))
    ))
  )

  def referencePrintInstrs: (Label, BlockInstrs) = (
    PrintReference.functionLabel,
    BlockInstrs(ListBuffer[Instr](
      Push(ListBuffer(R14_LR)),
      Mov(R1, resultRegister),
      Ldr(resultRegister, DataLabel(Label(PrintReference.msgName(0)))),
      Add(resultRegister, resultRegister, Imm_Int(SIZE_ADDR)),
      Bl(Label("printf")),
      Mov(resultRegister, Imm_Int(RESET_INT)),
      Bl(Label("fflush")),
      Pop(ListBuffer(R15_PC))
    ))
  )

  def newLinePrintInstrs: (Label, BlockInstrs) = (
    PrintLn.functionLabel,
    BlockInstrs(ListBuffer[Instr](
      Push(ListBuffer(R14_LR)),
      Ldr(resultRegister, DataLabel(Label(PrintLn.msgName(0)))),
      Add(resultRegister, resultRegister, Imm_Int(SIZE_ADDR)),
      Bl(Label("puts")),
      Mov(resultRegister, Imm_Int(RESET_INT)),
      Bl(Label("fflush")),
      Pop(ListBuffer(R15_PC))
    ))
  )
}
