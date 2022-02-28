package backend.codeGeneration

import frontend.AST._
import backend.Operand.{Register, RegAdd}
import backend.CodeGen._
import scala.collection.mutable.ListBuffer
import backend.Opcodes.{Instr, Mov, Ldr}
import backend.codeGeneration.ExpressionGen.transExp

object PairsGen {
  def transPairElem(ident: Ident, pos: Int, rd: Register): ListBuffer[Instr] = {
    val instructions = ListBuffer.empty[Instr]
    instructions ++= transExp(ident, rd)
    instructions += Mov(resultRegister, rd)
    // instructions += Bl(addRTE(NPE))
    
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

}
