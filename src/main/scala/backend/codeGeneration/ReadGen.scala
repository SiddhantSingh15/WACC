package backend.codeGeneration

import backend.codeGeneration.ArraysGen.transArrayElem
import backend.codeGeneration.PairsGen.transPairElem
import backend.CodeGen._
import backend.DefinedFuncs.PreDefinedFuncs.{ReadInt, ReadChar}
import backend.Opcodes._
import backend.Operand._
import frontend.AST._
import backend.ReadInstr.{charRead, intRead}
import scala.collection.mutable.ListBuffer

object ReadGen {
  private def getPairElemType(t: Type, pos: Int): Type = t match {
    case Pair(PairElemPair, PairElemPair) => null
    case Pair(PairElemPair, PairElemWithType(baseType))                     =>
      if (pos == 1) null else baseType
    case Pair(PairElemWithType(baseType), PairElemPair)                     =>
      if (pos == 1) baseType else null
    case Pair(PairElemWithType(baseTypeFst), PairElemWithType(baseTypeSnd)) =>
      if (pos == 1) baseTypeFst else baseTypeSnd
    case _                                                                  => 
      null
  }

  private def readBranch(t: Type): Instr = t match {
    case CharType =>
      preDefFuncTable.addFunction(
        charRead(dataTable.addData(ReadChar.msgs(0)))
      )
      Bl(ReadChar.functionLabel)
    case Int      =>
      preDefFuncTable.addFunction(
        intRead(dataTable.addData(ReadInt.msgs(0)))
      )
      Bl(ReadInt.functionLabel)
    // Semantically incorrect
    case _        => 
      null
  }

  def transReadPairElem(pe: PairElem, pos: Int): ListBuffer[Instr] = {
    val freeReg = saveReg()
    val ident: Ident = pe.expr match {
      case id: Ident => id
      case _         => null
    }
    val instructions = transPairElem(ident, pos, freeReg)
    val (_, pairType) = symbTable(ident)
    val t = getPairElemType(pairType, pos)
    // value must be in R0 for branch
    instructions += Mov(resultRegister, freeReg)
    restoreReg(freeReg)
    instructions += readBranch(t)
    instructions
  }

  private def transReadIdent(ident: Ident): ListBuffer[Instr] = {
    val instructions = ListBuffer.empty[Instr]
    val freeReg = saveReg()
    val (spIndex, identType) = symbTable(ident)
    val spOffset = stackPointer - spIndex
    instructions += Add(
      freeReg,
      R13_SP,
      Imm_Int(spOffset)
    )
    // variable must be in R0 for the branch
    instructions += Mov(resultRegister, freeReg)
    restoreReg(freeReg)
    instructions += readBranch(identType)
    instructions
  }

  def transReadArrayElem(ae: ArrayElem): ListBuffer[Instr] = {
    val ArrayElem(ident, exprs) = ae
    val instructions = ListBuffer.empty[Instr]
    val resReg = saveReg()
    // Handles nested arrays
    val (instrs, _) = transArrayElem(ident, exprs, resReg)
    instructions ++= instrs
    // value must be in R0 for branch
    instructions += Mov(resultRegister, resReg)
    restoreReg(resReg)
    // Gets base type of the arrayElem
    val t = getExprType(ae)
    instructions += readBranch(t)
    instructions
  }

  def transRead(lhs: AssignLHS): ListBuffer[Instr] = {
    lhs match {
      case ident: Ident  => transReadIdent(ident)
      case ae: ArrayElem => transReadArrayElem(ae)
      case fst: Fst      => transReadPairElem(fst, 1)
      case snd: Snd      => transReadPairElem(snd, 2)
    }
  }
}