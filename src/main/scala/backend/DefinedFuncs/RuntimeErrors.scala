package backend.DefinedFuncs

import backend.DefinedFuncs.PreDefinedFuncs._
import backend.DefinedFuncs.PrintInstrs._
import backend.Operand._
import backend.Condition._
import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import backend.CodeGen._
import backend.CodeGeneration.CodeGenHelper._


import backend.Condition.{EQ, NE}

import scala.collection.mutable.ListBuffer

object RuntimeErrors {

  private val ERROR_EXIT_CODE = -1

  /*
   * Add RunTimeError to dataTable and funcTable.
   */
  def addRTE(err: PreDefFunc): Label = {
    preDefFuncTable.addFunction(RuntimeError.function.get)
    preDefFuncTable.addFunction(stringPrintInstrs)
    dataTable.addLabel("msg_string", "%.*s\\0")
    for(n <- 0 until err.msgs.length){
      dataTable.addLabel(err.msgName(n), err.msgs(n))
    }
    preDefFuncTable.addFunction(err.function.get)
    err.functionLabel

  }

  /*
   * Raises RTE and adds the ARM machine code to the list of instructions.
   */
  def throwRuntimeError: (Label, BlockInstrs) = {
    (
      RuntimeError.functionLabel,
      BlockInstrs(ListBuffer[Instr](
        Bl(Label("p_print_string")),
        Mov(resultRegister, Imm_Int(ERROR_EXIT_CODE)),
        Bl(Label("exit"))
      ))
    )
  }

  /* 
   * Checks for illegal array accesses. 
   */
  def checkArrayBounds: (Label, BlockInstrs) = {
    (
      ArrayBounds.functionLabel,
      BlockInstrs(ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE_INT)),
        LdrCond(LT, resultRegister, DataLabel(Label(ArrayBounds.msgName(0)))),
        BranchLinkCond(LT, RuntimeError.functionLabel),
        Ldr(R1, RegAdd(R1)),
        Cmp(resultRegister, R1),
        LdrCond(CS, resultRegister, DataLabel(Label(ArrayBounds.msgName(1)))),
        BranchLinkCond(CS, RuntimeError.functionLabel),
        Pop(ListBuffer(R15_PC))
      ))
    )
  }

  /* 
   * Adds illegal division by zero error instructions.
   */
  def checkDivideByZero: (Label, BlockInstrs) = {
    (
      DivideByZero.functionLabel,
      BlockInstrs(ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(R1, Imm_Int(FALSE_INT)),
        LdrCond(EQ, resultRegister, DataLabel(Label(DivideByZero.msgName(0)))),
        BranchLinkCond(EQ, RuntimeError.functionLabel),
        Pop(ListBuffer(R15_PC))
      ))
    )
  }

  /*
   * Adds integer overflow to instructions list.
   */
  def throwOverflowError: (Label, BlockInstrs) = {
    (
      Overflow.functionLabel,
      BlockInstrs(ListBuffer[Instr](
        Ldr(resultRegister, DataLabel(Label(Overflow.msgName(0)))),
        Bl(RuntimeError.functionLabel)
      ))
    )
  }

  /*
   * Adds instruction to free a pair.
   */
  def freePair: (Label, BlockInstrs) = {
    (
      FreePair.functionLabel,
      BlockInstrs(ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE_INT)),
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
      ))
    )
  }

  /*
   * Adds instruction to free an array.
   */
  def freeArray: (Label, BlockInstrs) = {
    (
      FreeArray.functionLabel,
      BlockInstrs(ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE_INT)),
        LdrCond(EQ, resultRegister, DataLabel(Label(FreeArray.msgName(0)))),
        BranchCond(EQ, RuntimeError.functionLabel),
        Bl(Label("free")),
        Pop(ListBuffer(R15_PC))
      ))
    )
  }

  /*
   * Add NPE instructions to list of instructions.
   */
  def checkNullPointer: (Label, BlockInstrs) = {
    (
      NPE.functionLabel,
      BlockInstrs(ListBuffer[Instr](
        Push(ListBuffer(R14_LR)),
        Cmp(resultRegister, Imm_Int(FALSE_INT)),
        LdrCond(EQ, resultRegister, DataLabel(Label(NPE.msgName(0)))),
        BranchLinkCond(EQ, RuntimeError.functionLabel),
        Pop(ListBuffer(R15_PC))
      ))
    )
  }
}
