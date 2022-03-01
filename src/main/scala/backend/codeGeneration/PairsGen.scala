package backend.codeGeneration

import frontend.AST._
import backend.Operand.{Register, RegAdd}
import backend.CodeGen._
import scala.collection.mutable.ListBuffer
import backend.Opcodes.{Instr, Mov, Ldr, Bl}
import backend.codeGeneration.ExpressionGen.transExp
import backend.DefinedFuncs.RuntimeErrors._
import backend.DefinedFuncs.PreDefinedFuncs.{NPE}
import backend.codeGeneration.ExpressionGen._

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
        case _                                => 
      }
    } else {
      t match {
        case Pair(_, PairElemWithType(sType)) => sType
        case Pair(PairElemPair, _)            => Pair(null, null)
        case _                                => 
      }
    }




    // val (bytes, instrs) = assignRHS(pElemType, rhs, rd)
    // instructions ++= instrs
    val nextRegister = saveReg()
    instructions ++= transPairElem(ident, pos, nextRegister)
    // instructions += Str(bytes, rd, nextRegister, 0)
    addFreeReg(nextRegister)
    instructions
  }


  def translateAssignRHSPair(tpe : Type, fst : Expr, snd: Expr, register : Register) : ListBuffer[Instr] = {
    val instrs = ListBuffer.empty[Instr]
    val Pair(typeOne, typeTwo) = tpe 
    val nextRegister = saveReg()

    instrs += Ldr(resultRegister, Load_Mem(2 * PAIR_SIZE))
    instrs += Bl(Label("malloc"))
    instrs += Mov(register, resultRegister)

    instrs += transExp(fst, nextRegister)

    instrs += Ldr(resultRegister ,Load_Mem(getPairElemTypeSize(typeOne)))

    instrs += Bl(Label("malloc"))

    instrs += Str(
      isBytePair(p, 0),
      nextRegister,
      resultRegister,
      NO_OFFSET
    )

    instrs += Str(resultRegister , RegAdd(register))

    instrs ++= transExp(snd, nextRegister)

    instrs += Ldr(resultRegister, Load_Mem(getPairElemTypeSize(typeTwo)))
    instrs += Bl(Label("malloc"))

    instrs += Str(
      isBytePair(p, 1),
      nextRegister,
      resultRegister,
      NO_OFFSET
    )

    freeRegister(nextRegister)
    instrs += Str(resultRegister, register, PAIR_SIZE)
    instrs

  }

  private def getPairTypeSize(tpe : PairElemType) : Int = {
    tpe match {
      case PairElemPair => PAIR_SIZE
      case PairElemWithType(t) => getTypeSize(t)
    }
  }

}

