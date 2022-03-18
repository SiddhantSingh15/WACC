package backend

import java.io.{File, FileWriter}
import backend.Opcodes._
import backend.Operand._
import backend.Condition._
import backend.CodeGen.{TRUE_INT, FALSE_INT}
import backend.DefinedFuncs.PreDefinedFuncs._
import backend.CodeGen.resultRegister
import scala.collection._
import backend.DefinedFuncs.PreDefinedFuncs.DivideByZero
import scala.collection.mutable.ListBuffer
import scala.math._

object InstrEval {
  val Is_Byte = true
  val Not_Byte = false

  /* Blocks of instructions that should be ignored */
  val ignoreBlocks = mutable.ListBuffer.empty[Label]

  /* Optimises a block of instructions */
  def optimiseBlock(
      block: (Label, BlockInstrs)
  ): (Label, BlockInstrs) = {
      val (label, instructions) = block
      val optimised = mutable.ListBuffer.empty[Instr]
      val instrs = mutable.ListBuffer.empty[Instr]
      instrs.addAll(instructions.instrBlock)
      // Store optimised instructions into optimised
      optimise(instrs, optimised)
      val returnBlock = BlockInstrs(optimised)
      (label, returnBlock)
  }

  /* Continues optimise recursively */
  def continueOptimise(
      cur: Instr,
      remainingHead: Instr,
      remainingTail: mutable.ListBuffer[Instr],
      optimised: mutable.ListBuffer[Instr]
  ): Unit = {
      optimised += cur
      optimise(remainingHead, remainingTail, optimised)
  }

  /* Overloaded optimise function that is recursively called to store
    optimised instructions in optimised */
  def optimise(
      instructions: mutable.ListBuffer[Instr],
      optimised: mutable.ListBuffer[Instr]
  ): Unit = {
      if (!instructions.isEmpty) {
          optimise(instructions.head, instructions.tail, optimised)
      }
  }

  /* Overloaded optimise function that is recursively called to store
    optimised instructions in OPTIMISED */
  def optimise(
      cur: Instr,
      instructions: mutable.ListBuffer[Instr],
      optimised: mutable.ListBuffer[Instr]
  ): Unit = {
      if (instructions.isEmpty) {
          optimised += cur
      } else {
          val remainingHead = instructions.head
          val remainingTail = instructions.tail
          (cur, remainingHead) match {
              case (Mov(r1, op1), Mov(rd, r2)) =>
              // Remove redundance Mov Instructions
              peepMov(r1, op1, rd, r2, remainingTail, optimised)
              case (Mov(r1, op1), Cmp(rd, op2)) =>
              // Check for redundant compare branches
              peepBranch(r1, op1, rd, op2, remainingTail, optimised)
              case (Ldr(r1, op1), Ldr(r2, op2)) =>
              // Potential strong operation
              peepStrong(r1, op1, r2, op2, remainingTail, optimised)
              case (Str(r1, op1), Ldr(r2, op2)) =>
              // Potential useless Load
              peepStrLdr(r1, op1, r2, op2, remainingTail, optimised, Not_Byte)
              case (StrB(r1, op1), LdrSB(r2, op2)) =>
              // Potential useless Load
              peepStrLdr(r1, op1, r2, op2, remainingTail, optimised, Is_Byte)
              case _ =>
              continueOptimise(cur, remainingHead, remainingTail, optimised)
          }
      }
  }

  /* Call optimise on all blocks necessary */
  def optimiseBlocks(
      blocks: List[(Label, BlockInstrs)]
  ): List[(Label, BlockInstrs)] = {
      val returnBlocks = mutable.ListBuffer.empty[(Label, BlockInstrs)]
      for (b <- blocks) {
          val (Label(name), _) = b
          // Ignore blocks that are not required
          if (!ignoreBlocks.contains(Label(name))) {
              name.take(2) match {
              // Not necassary to optimise predefined blocks
                  case "p_" => returnBlocks += b
                  case _    => returnBlocks += optimiseBlock(b)
              }
          }
      }
      returnBlocks.toList
  }

  /* Removes redundant branches */
  def peepBranch(
      r1: Register,
      op1: Operand,
      rd: Register,
      op2: Operand,
      remainingTail: mutable.ListBuffer[Instr],
      optimised: mutable.ListBuffer[Instr]
  ): Unit = {
      if (r1 == rd) {
          (op1, op2) match {
              // Comparison is true
              case (Imm_Int(FALSE_INT), Imm_Int(FALSE_INT)) |
                  (Imm_Int(TRUE_INT), Imm_Int(TRUE_INT)) =>
                  remainingTail.head match {
                  case BranchCond(EQ, label) =>
                      // Branch to necessary label
                      optimised += Branch(label)
                  case _ =>
                      continueOptimise(
                      Mov(r1, op1),
                      Cmp(rd, op2),
                      remainingTail,
                      optimised
                      )
                  }
              // Comparison is false
              case (Imm_Int(FALSE_INT), Imm_Int(TRUE_INT)) |
                  (Imm_Int(TRUE_INT), Imm_Int(FALSE_INT)) =>
              remainingTail.head match {
                  case BranchCond(EQ, label) =>
                  // LABEL will never be required
                  ignoreBlocks += label
                  // Optimise rest of the instructions
                  optimise(remainingTail.tail, optimised)
                  case _ =>
                  continueOptimise(
                      Mov(r1, op1),
                      Cmp(rd, op2),
                      remainingTail,
                      optimised
                  )
              }
              case _ =>
              continueOptimise(Mov(r1, op1), Cmp(rd, op2), remainingTail, optimised)
          }
      } else {
          continueOptimise(Mov(r1, op1), Cmp(rd, op2), remainingTail, optimised)
      }
  }

  /* Remove redundant Mov(r1,op1), Mov(rd, r1) -> Mov(rd, op1) */
  def peepMov(
      r1: Register,
      op1: Operand,
      rd: Register,
      r2: Operand,
      instructions: mutable.ListBuffer[Instr],
      optimised: mutable.ListBuffer[Instr]
  ): Unit = {
      if (r1 == r2) {
      // Check destReg != op1
          if (rd != op1) {
              optimise(Mov(rd, op1), instructions, optimised)
          } else {
              // If rd == op1, no need to add intructions
              optimise(instructions.head, instructions.tail, optimised)
          }
      } else {
      continueOptimise(Mov(r1, op1), Mov(rd, r2), instructions, optimised)
      }
  }

  /* Removes redundant Load instructions */
  def peepStrLdr(
      r1: Register,
      op1: Address,
      r2: Register,
      op2: Operand,
      instructions: mutable.ListBuffer[Instr],
      optimised: mutable.ListBuffer[Instr],
      isByte: Boolean
  ): Unit = {
      if (r1 == r2 && op1 == op2) {
          // Skip over the Load intruction
          optimise(Str(isByte, r1, op1), instructions, optimised)
      } else {
          continueOptimise(
          Str(isByte, r1, op1),
          Ldr(isByte, r2, op2),
          instructions,
          optimised
          )
      }
  }

  val LOG_ERROR = -1
  private val Shift_Factor = 2.0
  private val Drop_Branch_Insts = 3

  /* Get log2 of i, returns LOG_ERROR if I == 0 or value is not an Integer */
  def getShiftAmount(i: Int): Int = {
      if (i <= 0) {
          LOG_ERROR
      } else {
          val dVal = log(i) / log(Shift_Factor)
          if (dVal == floor(dVal)) {
              dVal.toInt
          } else {
              LOG_ERROR
          }
      }
  } 

    /* Reduces strength of a division if possible */
  def divisionReduc(
      r1: Register,
      op1: Operand,
      r2: Register,
      op2: Operand,
      instructions: mutable.ListBuffer[Instr],
      optimised: mutable.ListBuffer[Instr]
  ): Unit = {
    val load1 = Ldr(r1, op1)
    val load2 = Ldr(r2, op2)
    op2 match {
      // If dividing by 0, call DIVIDE_BY_ZERO runtime error
      case Load_Mem(0) =>
        // Call DIVIDE_BY_ZERO runtime error
        optimised += Ldr(
          resultRegister,
          DataLabel(Label(DivideByZero.msgName(0)))
        )
        optimised += Bl(RuntimeError.functionLabel)
      case Load_Mem(n) =>
        val shiftAmount = getShiftAmount(n)
        op1 match {
          case Load_Mem(n1) =>
            if (
              (shiftAmount != LOG_ERROR) && (getShiftAmount(n1) != LOG_ERROR)
            ) {
              instructions.head match {
                // Get the destination reg for the division
                case Mov(rd, _) =>
                  // Instructions in order to shift rather than
                  //  using __aeabi_idiv
                  val newInstructions = instructions.drop(4)
                  // Cons Instructions to the head of the list
                  Mov(rd, r1) +=: newInstructions
                  if (shiftAmount != 0) {
                    // Add(r1, r1, r2) +=: newInstructions
                    Mov(r1, LSR(r1, Imm_Int(shiftAmount))) +=: newInstructions
                  }
                  // Mov(r2, ASR(r1, ImmInt(31))) +=: newInstructions
                  load1 +=: newInstructions
                  // Optimise instructinos from NEWINSTRUCTIONS
                  optimise(newInstructions, optimised)
                case _ =>
                  continueOptimise(load1, load2, instructions, optimised)
              }
            } else {
              continueOptimise(load1, load2, instructions, optimised)
            }
          case _ => continueOptimise(load1, load2, instructions, optimised)
        }
      case _ =>
        continueOptimise(load1, load2, instructions, optimised)
    }
  }

    /* Reduces strength of a multiplication if possible */
  def multiplyReduc(
      r1: Register,
      op1: Operand,
      r2: Register,
      op2: Operand,
      instructions: mutable.ListBuffer[Instr],
      optimised: mutable.ListBuffer[Instr]
  ): Unit = {
    val load1 = Ldr(r1, op1)
    val load2 = Ldr(r2, op2)
    // Ensure SMULL instruction
    if (instructions.size >= 4) {
      var newInstructions = instructions.drop(3)
      (op1, op2) match {
        // If multiplying by 0, load 0 into the destination register
        case (Load_Mem(0), _) | (_, Load_Mem(0)) =>
          optimise(Ldr(r1, Load_Mem(0)), instructions.drop(3), optimised)
        case (Load_Mem(n1), Load_Mem(n2)) =>
          val shiftAmount1 = getShiftAmount(n1)
          val shiftAmount2 = getShiftAmount(n2)
          // Get largest shift to optimise division
          if (shiftAmount1 > shiftAmount2 && shiftAmount1 != LOG_ERROR) {
            shiftOptimise(shiftAmount1, instructions, r1, r2, op2, optimised)
          } else if (
            shiftAmount1 <= shiftAmount2 && shiftAmount2 != LOG_ERROR
          ) {
            shiftOptimise(shiftAmount2, instructions, r1, r2, op1, optimised)
          } else {
            continueOptimise(load1, load2, instructions, optimised)
          }
        case (Load_Mem(n), _) =>
          val shiftAmount = getShiftAmount(n)
          if (shiftAmount != LOG_ERROR) {
            shiftOptimise(shiftAmount, instructions, r1, r2, op2, optimised)
          } else {
            continueOptimise(load1, load2, instructions, optimised)
          }
        case (_, Load_Mem(n)) =>
          val shiftAmount = getShiftAmount(n)
          if (shiftAmount != LOG_ERROR) {
            shiftOptimise(shiftAmount, instructions, r1, r2, op1, optimised)
          } else {
            continueOptimise(load1, load2, instructions, optimised)
          }
        case _ =>
          continueOptimise(load1, load2, instructions, optimised)
      }
    } else {
      continueOptimise(load1, load2, instructions, optimised)
    }
  }

  // Adds shift instuctions and continues to optimise
  def shiftOptimise(
      shiftAmount: Int,
      instructions: mutable.ListBuffer[Instr],
      r1: Register,
      r2: Register,
      op1: Operand,
      optimised: mutable.ListBuffer[Instr]
  ): Unit = {
    var newInstructions = instructions.drop(Drop_Branch_Insts)
    if (shiftAmount != 0) {
      newInstructions = instructions.tail
      Mov(r1, ASL(r1, Imm_Int(shiftAmount))) +=: newInstructions
      Mov(r2, ASR(r1, Imm_Int(31))) +=: newInstructions
    }
    Ldr(r1, op1) +=: newInstructions

    // Optimise instructinos from NEWINSTRUCTIONS
    optimise(newInstructions, optimised)
  }

  /* Function for strength reduction */
  def peepStrong(
      r1: Register,
      op1: Operand,
      r2: Register,
      op2: Operand,
      instructions: mutable.ListBuffer[Instr],
      optimised: mutable.ListBuffer[Instr]
  ): Unit = {
    val load1 = Ldr(r1, op1)
    val load2 = Ldr(r2, op2)
    // Multiplication instruction
    if (instructions.head == SMul(r1, r2, r1, r2)) {
      multiplyReduc(r1, op1, r2, op2, instructions, optimised)
    } else if (instructions.size >= 4) {
      // Division instruction
      if (instructions.drop(3).head == Bl(Label("__aeabi_idiv"))) {
        divisionReduc(r1, op1, r2, op2, instructions, optimised)
      } else {
        continueOptimise(load1, load2, instructions, optimised)
      }
    } else {
      continueOptimise(load1, load2, instructions, optimised)
    }
  }
}
