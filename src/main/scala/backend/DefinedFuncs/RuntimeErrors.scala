package backend.DefinedFuncs

import backend.CodeGen.{
  dataTable,
  funcTable,
  resultRegister,
  FALSE
}

import backend.DefinedFuncs.PreDefinedFuncs._
import backend.DefinedFuncs.PrintInstrs._
import backend.Operand._
import backend.Condition._
import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import backend.CodeGen._


import backend.Condition.{EQ, NE}

import scala.collection.mutable.ListBuffer

object RuntimeErrors {

  private val ERROR_EXIT_CODE = -1

  def addRTE(err: PreDefFunc): Label = {
    preDefFuncTable.addFunction(RuntimeError.function)
    preDefFuncTable.addFunction(stringPrintInstrs)
    dataTable.addLabel("msg_string", "%.*s\\0")
    for(n <- 0 until err.msgs.length){
      dataTable.addLabel(err.msgName(n), err.msgs(n))
    }
    preDefFuncTable.addFunction(err.function)
    err.functionLabel

  }

  def throwRuntimeError: (Label, ListBuffer[Instr]) = {
    (
      RuntimeError.functionLabel,
      ListBuffer[Instr](
        Bl(Label("p_print_string")),
        Mov(resultRegister, Imm_Int(ERROR_EXIT_CODE)),
        Bl(Label("exit"))
      )
    )
  }

  def checkArrayBounds: (Label, ListBuffer[Instr]) = {
    (
      ArrayBounds.functionLabel,
      ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE)),
        LdrCond(LT, resultRegister, DataLabel(Label(ArrayBounds.msgName(0)))),
        BranchLinkCond(LT, RuntimeError.functionLabel),
        Ldr(R1, RegAdd(R1)),
        Cmp(resultRegister, R1),
        LdrCond(CS, resultRegister, DataLabel(Label(ArrayBounds.msgName(1)))),
        BranchLinkCond(CS, RuntimeError.functionLabel),
        Pop(ListBuffer(R15_PC))
      )
    )
  }

  def checkDivideByZero: (Label, ListBuffer[Instr]) = {
    (
      DivideByZero.functionLabel,
      ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(R1, Imm_Int(FALSE)),
        LdrCond(EQ, resultRegister, DataLabel(Label(DivideByZero.msgName(0)))),
        BranchLinkCond(EQ, RuntimeError.functionLabel),
        Pop(ListBuffer(R15_PC))
      )
    )
  }

  def throwOverflowError: (Label, ListBuffer[Instr]) = {
    (
      Overflow.functionLabel,
      ListBuffer[Instr](
        Ldr(resultRegister, DataLabel(Label(Overflow.msgName(0)))),
        Bl(RuntimeError.functionLabel)
      )
    )
  }

  def freePair: (Label, ListBuffer[Instr]) = {
    (
      FreePair.functionLabel,
      ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE)),
        LdrCond(EQ, resultRegister, DataLabel(Label(FreePair.msgName(0)))),
        BranchCond(EQ, RuntimeError.functionLabel),
        Push(ListBuffer(resultRegister)),
        Ldr(resultRegister, RegAdd(resultRegister)),
        Bl(Label("free")),
        Ldr(resultRegister, RegAdd(R13_SP)),
        Ldr(resultRegister, RegisterOffset(resultRegister, SIZE_ADDR)),
        Bl(Label("free")),
        Pop(ListBuffer(resultRegister)),
        Bl(Label("free")),
        Pop(ListBuffer(R15_PC))
      )
    )
  }

  def freeArray: (Label, ListBuffer[Instr]) = {
    (
      FreeArray.functionLabel,
      ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE)),
        LdrCond(EQ, resultRegister, DataLabel(Label(FreeArray.msgName(0)))),
        BranchCond(EQ, RuntimeError.functionLabel),
        Bl(Label("free")),
        Pop(ListBuffer(R15_PC))
      )
    )
  }

  def checkNullPointer: (Label, ListBuffer[Instr]) = {
    (
      NPE.functionLabel,
      ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE)),
        LdrCond(EQ, resultRegister, DataLabel(Label(NPE.msgName(0)))),
        BranchLinkCond(EQ, RuntimeError.functionLabel),
        Pop(ListBuffer(R15_PC))
      )
    )
  }
}
