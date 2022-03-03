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

  /* 
   * Translates the READ function and returns a list of instructions.
   */
  def transRead(lhs: AssignLHS): Unit = {
    lhs match {
      case ident: Ident  => transReadIdent(ident)
      case ae: ArrayElem => transReadArrayElem(ae)
      case fst: Fst      => transReadPairElem(fst, 1)
      case snd: Snd      => transReadPairElem(snd, 2)
    }
  }

  /*
   * Matches the type and returns the Bl instruction.
   */
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
    case _        => 
      null
  }

  /* 
   * Translates the Pair Element.
   */
  def transReadPairElem(pe: PairElem, pos: Int): Unit = {
    val freeReg = saveReg()
    val ident: Ident = pe.expr match {
      case id: Ident => id
      case _         => null
    }
    transPairElem(ident, pos, freeReg)
    val (_, pairType) = symbTable(ident)
    val t = getPairElemType(pairType, pos)
    // value must be in R0 for branch
    currInstructions += Mov(resultRegister, freeReg)
    restoreReg(freeReg)
    currInstructions += readBranch(t)
  }

  /* 
   * Translates the READ function identifier for Char and IntType only.
   */
  private def transReadIdent(ident: Ident): Unit = {

    val freeReg = saveReg()
    val (spIndex, identType) = symbTable(ident)
    val spOffset = stackPointer - spIndex
    currInstructions += Add(freeReg, R13_SP, Imm_Int(spOffset))
    currInstructions += Mov(resultRegister, freeReg)
    restoreReg(freeReg)
    currInstructions += readBranch(identType)
  }

  /*
   * Translates the array element and returns the list of instructions.
   */
  def transReadArrayElem(ae: ArrayElem): Unit = {
    val ArrayElem(ident, exprs) = ae
    val resReg = saveReg()
    transArrayElem(ident, exprs, resReg)
    currInstructions += Mov(resultRegister, resReg)
    restoreReg(resReg)
    val t = getExprType(ae)
    currInstructions += readBranch(t)
  }

  /*
   * Returns the type of the pair element.
   * If pos == 1, returns the type of the first pair element.
   * Otherwise, returns the type of the second pair element.
   */
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
}