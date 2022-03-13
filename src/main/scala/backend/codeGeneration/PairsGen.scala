package backend.CodeGeneration

import frontend.AST._
import backend.Operand.{Register, RegAdd, Load_Mem}
import backend.CodeGen._
import scala.collection.mutable.ListBuffer
import backend.DefinedFuncs.RuntimeErrors._
import backend.DefinedFuncs.PreDefinedFuncs.{NPE}
import backend.Opcodes._
import backend.CodeGeneration.ExpressionGen._
import backend.CodeGeneration.Assignments._
import backend.CodeGeneration.CodeGenHelper._

object PairsGen {

  /*
   * Translates the PairElem type.
   * Takes an Ident, pair position and a register.
   * Returns a list of instructions.
   */
  def transPairElem(ident: Ident, pos: Int, rd: Register): Unit = {
    transExp(ident, rd)
    currInstructions.addAll(ListBuffer[Instr](
      Mov(resultRegister, rd),
      Bl(addRTE(NPE))
    ))
    
    if (pos == 1) {
      currInstructions.add(Ldr(rd, RegAdd(rd)))
    } else {
      currInstructions.add(Ldr(rd, rd, SIZE_PAIR))
    }
  }

  /* 
   * Loads the pair into RD
   */
  def transPairAssign(rhs: AssignRHS, ident: Ident, pos: Int, rd: Register): Unit = {
    
    val (i, t) = symbTable(ident)
    val pElemType = 
    if (pos == 1) {
      t match {
        case Pair(PairElemWithType(fType), _) => fType
        case Pair(PairElemPair, _)            => Pair(null, null)
        case _                                => ???
      }
    } else {
      t match {
        case Pair(_, PairElemWithType(sType)) => sType
        case Pair(_, PairElemPair)            => Pair(null, null)
        case _                                => ???
      }
    }
    val isByte = transAssignRHS(pElemType, rhs, rd)
    val nextRegister = saveReg()
    transPairElem(ident, pos, nextRegister)
    currInstructions.add(Str(isByte, rd, nextRegister, NO_OFFSET))
    restoreReg(nextRegister)
  }

  /* 
   * Translates Pair assignment.
   * Takes the type of the fst and snd, and a register
   * Returns the instructions list for the assignment.
   */
  def transAssignRHSPair(fst : Expr, snd: Expr, register : Register) : Unit = {

    val typeOne = getExprType(fst)
    val typeTwo = getExprType(snd)
    val nextRegister = saveReg()

    currInstructions.addAll(ListBuffer[Instr](
      Ldr(resultRegister, Load_Mem(2 * SIZE_PAIR)),
      Bl(Label("malloc")),
      Mov(register, resultRegister)
    ))

    transExp(fst, nextRegister)

    currInstructions.addAll(ListBuffer[Instr](
      Ldr(resultRegister, Load_Mem(getTypeSize(typeOne))),
      Bl(Label("malloc")),
      Str(isByte(typeOne), nextRegister, resultRegister, NO_OFFSET),
      Str(resultRegister , RegAdd(register))
    ))

    transExp(snd, nextRegister)
    currInstructions.addAll(ListBuffer[Instr](
      Ldr(resultRegister, Load_Mem(getTypeSize(typeTwo))),
      Bl(Label("malloc")),
      Str(isByte(typeTwo), nextRegister, resultRegister, NO_OFFSET)
    ))

    restoreReg(nextRegister)
    currInstructions.add(Str(resultRegister, register, SIZE_PAIR))
  }

  /*
   * Loads pair of name "ident" into passed in rd.
   */
  def getPairElem(ident: Ident, pos: Int, rd: Register): Unit = {
    val (_, tpe) = symbTable(ident)
    currInstructions.add(Ldr(isBytePair(tpe, pos), rd, rd, NO_OFFSET))
  }

  /*
   * Returns the size of the pair type.
   */
  private def getPairTypeSize(tpe : PairElemType) : Int = {
    tpe match {
      case PairElemPair        => SIZE_PAIR
      case PairElemWithType(t) => getTypeSize(t)
      case _                   => ???
    }
  }

  /* 
   * Helper function for getPairElem().
   * Returns true if the passed in type is byte sized.
   */
  private def isBytePair(tpe : Type, pos : Int) = {
    tpe match {
      case Pair(PairElemWithType(a), PairElemWithType(b)) =>
        if (pos == 1) {
          isByte(a)
        } else {
          isByte(b)
        }
      case _                                              => 
        false
    }
  }
}

