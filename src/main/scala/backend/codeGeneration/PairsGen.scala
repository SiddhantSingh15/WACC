package backend.codeGeneration

import frontend.AST._
import backend.Operand.{Register, RegAdd, Load_Mem}
import backend.CodeGen._
import scala.collection.mutable.ListBuffer
import backend.Opcodes.{Instr, Mov, Ldr, Bl}
import backend.codeGeneration.ExpressionGen.transExp
import backend.DefinedFuncs.RuntimeErrors._
import backend.DefinedFuncs.PreDefinedFuncs.{NPE}
import backend.codeGeneration.ExpressionGen._
import backend.Opcodes._
import parsley.internal.deepembedding.StringLiteral
import backend.codeGeneration.ExpressionGen._
import backend.codeGeneration.Assignments._

object PairsGen {
  def transPairElem(ident: Ident, pos: Int, rd: Register): ListBuffer[Instr] = {
    val instructions = ListBuffer.empty[Instr]
    instructions ++= transExp(ident, rd)
    instructions += Mov(resultRegister, rd)
    instructions += Bl(addRTE(NPE))
    
    if (pos == 1) {
      instructions += Ldr(rd, RegAdd(rd))
    } else {
      instructions += Ldr(rd, rd, SIZE_PAIR)
    }
    instructions
  }

  def transPairAssign(rhs: AssignRHS, ident: Ident, pos: Int, rd: Register): ListBuffer[Instr] = {
    val instructions = ListBuffer.empty[Instr]
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
        case Pair(PairElemPair, _)            => Pair(null, null)
        case _                                => ???
      }
    }
    val (isByte, instrs) = transAssignRHS(pElemType, rhs, rd)
    instructions ++= instrs
    val nextRegister = saveReg()
    instructions ++= transPairElem(ident, pos, nextRegister)
    instructions += Str(isByte, rd, nextRegister, NO_OFFSET)
    restoreReg(nextRegister)
    instructions
  }

  def transAssignRHSPair(tpe : Type, fst : Expr, snd: Expr, register : Register) : ListBuffer[Instr] = {
    val instrs = ListBuffer.empty[Instr]
    val Pair(typeOne, typeTwo) = tpe 
    val nextRegister = saveReg()

    instrs += Ldr(resultRegister, Load_Mem(2 * SIZE_PAIR))
    instrs += Bl(Label("malloc"))
    instrs += Mov(register, resultRegister)

    instrs ++= transExp(fst, nextRegister)

    instrs += Ldr(resultRegister ,Load_Mem(getPairTypeSize(typeOne)))
    instrs += Bl(Label("malloc"))
    instrs += Str(
      isBytePair(tpe, 1),
      nextRegister,
      resultRegister,
      NO_OFFSET
    )

    instrs += Str(resultRegister , RegAdd(register))

    instrs ++= transExp(snd, nextRegister)

    instrs += Ldr(resultRegister, Load_Mem(getPairTypeSize(typeTwo)))
    instrs += Bl(Label("malloc"))

    instrs += Str(
      isBytePair(tpe, 2),
      nextRegister,
      resultRegister,
      NO_OFFSET
    )

    restoreReg(nextRegister)
    instrs += Str(resultRegister, register, SIZE_PAIR)
    instrs
  }

  def getPairElem(ident: Ident, pos: Int, rd: Register): Instr = {
    val (_, tpe) = symbTable(ident)
    Ldr(isBytePair(tpe, pos), rd, rd, NO_OFFSET)
  }

  private def getPairTypeSize(tpe : PairElemType) : Int = {
    tpe match {
      case PairElemPair => SIZE_PAIR
      case PairElemWithType(t) => getTypeSize(t)
      case _ => ???
    }
  }

  private def isBytePair(tpe : Type, pos : Int) = {
    tpe match {
      case Pair(PairElemWithType(a), PairElemWithType(b)) =>
        if (pos == 1) {
          isByte(a)
        } else {
          isByte(b)
        }
      case _ 
        => false
    }
  }
}

