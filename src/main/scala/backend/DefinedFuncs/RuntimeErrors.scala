package backend.DefinedFuncs

import backend.CodeGen.{
  dataTable,
  funcTable,
  resultRegister,
  FALSE_INT
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

  /* Adds a runtime error ERR to the function table. */
  def addRuntimeError(err: PreDefFunc): Label = {
    funcTable.addFunction(RuntimeError.func)
    funcTable.addFunction(stringPrintInstrs)
    dataTable.addLabel("msg_string", "%.*s\\0")
    for(i <- 0 until err.msgs.length){
      dataTable.addLabel(err.msgName(i), err.msgs(i))
    }
    funcTable.addFunction(err.func)
    err.funcLabel

  }

  /* Throws a runtime error by adding the appropiate instructions to the
     internal representation. */
  def throwRuntimeError: (Label, ListBuffer[Instr]) = {
    (
      RuntimeError.funcLabel,
      ListBuffer[Instr](
        Bl(Label("p_print_string")),
        Mov(resultRegister, Imm_Int(ERROR_EXIT_CODE)),
        Bl(Label("exit"))
      )
    )
  }

  /* Checks the bounds of a given array, branching to a runtime error if
     necessary. */
  def checkArrayBounds: (Label, ListBuffer[Instr]) = {
    (
      ArrayBounds.funcLabel,
      ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE_INT)),
        LdrCond(LT, resultRegister, DataLabel(Label(ArrayBounds.msgName(0)))),
        BranchLinkCond(LT, RuntimeError.funcLabel),
        Ldr(R1, RegAdd(R1)),
        Cmp(resultRegister, R1),
        LdrCond(CS, resultRegister, DataLabel(Label(ArrayBounds.msgName(1)))),
        BranchLinkCond(CS, RuntimeError.funcLabel),
        Pop(ListBuffer(R15_PC))
      )
    )
  }

  /* Checks an expression to determine whether an expression has been divided
     by 0, branching to a runtime error if necessary. */
  def checkDivideByZero: (Label, ListBuffer[Instr]) = {
    (
      DivideByZero.funcLabel,
      ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(R1, Imm_Int(FALSE_INT)),
        LdrCond(EQ, resultRegister, DataLabel(Label(DivideByZero.msgName(0)))),
        BranchLinkCond(EQ, RuntimeError.funcLabel),
        Pop(ListBuffer(R15_PC))
      )
    )
  }

  /* Checks an expression to determine whether an int has overflown, branching
     to a runtime error if necessary. */
  def throwOverflowError: (Label, ListBuffer[Instr]) = {
    (
      Overflow.funcLabel,
      ListBuffer[Instr](
        Ldr(resultRegister, DataLabel(Label(Overflow.msgName(0)))),
        Bl(RuntimeError.funcLabel)
      )
    )
  }

  /* Free a given pair, branching to a runtime error if necessary. */
  def freePair: (Label, ListBuffer[Instr]) = {
    (
      FreePair.funcLabel,
      ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE_INT)),
        LdrCond(EQ, resultRegister, DataLabel(Label(FreePair.msgName(0)))),
        BranchCond(EQ, RuntimeError.funcLabel),
        Push(ListBuffer(resultRegister)),
        Ldr(resultRegister, RegAdd(resultRegister)),
        Bl(Label("free")),
        Ldr(resultRegister, RegAdd(R13_SP)),
        Ldr(resultRegister, RegisterOffset(resultRegister, ADDRESS_SIZE)),
        Bl(Label("free")),
        Pop(ListBuffer(resultRegister)),
        Bl(Label("free")),
        Pop(ListBuffer(R15_PC))
      )
    )
  }

  /* Free a given array, branching to a runtime error if necessary. */
  def freeArray: (Label, ListBuffer[Instr]) = {
    (
      FreeArray.funcLabel,
      ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE_INT)),
        LdrCond(EQ, resultRegister, DataLabel(Label(FreeArray.msgName(0)))),
        BranchCond(EQ, RuntimeError.funcLabel),
        Bl(Label("free")),
        Pop(ListBuffer(R15_PC))
      )
    )
  }

  /* Checks if attempting to defererence a null pointer, branching to a
     runtime error if necessary. */
  def checkNullPointer: (Label, ListBuffer[Instr]) = {
    (
      NullPointer.funcLabel,
      ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE_INT)),
        LdrCond(EQ, resultRegister, DataLabel(Label(NullPointer.msgName(0)))),
        BranchLinkCond(EQ, RuntimeError.funcLabel),
        Pop(ListBuffer(R15_PC))
      )
    )
  }
}
