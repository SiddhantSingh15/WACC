package backend.CodeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._
import backend.CodeGen._
import frontend.SymbolTable
import backend.CodeGeneration.ExpressionGen._
import backend.CodeGeneration.PairsGen._
import backend.CodeGeneration.Functions._
import backend.CodeGeneration.ArraysGen._
import backend.CodeGeneration.CodeGenHelper._

import scala.collection.mutable.ListBuffer


object Assignments {

  /*Translating declaration of new variable to ARM language*/
  def translateDeclaration(t: Type, id : Ident, rhs : AssignRHS): Unit = {
    scopeSP += getTypeSize(t)
    symbTable.add(id, scopeSP, t)
    val spOffset = currSP - scopeSP
    val freeRegister = saveReg()
    val isByte = transAssignRHS(t, rhs, freeRegister)
    currInstructions += Str(isByte, freeRegister, R13_SP, spOffset)
    restoreReg(freeRegister)
  }

  /*Translating assignment of a variable to ARM language*/
  def transAssignment(lhs: AssignLHS, rhs: AssignRHS): Unit = {
    val freeRegister = saveReg()

    lhs match {
      case id : Ident                   =>
        val (index, t) = symbTable(id)
        val isByte= transAssignRHS(t, rhs, freeRegister)
        val spOffset = currSP - index  
        currInstructions += Str(isByte, freeRegister, R13_SP, spOffset)
      case Fst(id : Ident)              => 
        transPairAssign(rhs, id, 1, freeRegister)
      case Snd(id : Ident)              => 
        transPairAssign(rhs, id, 2, freeRegister)
      case x@ArrayElem(ident, exprList) =>
        transAssignRHS(getExprType(x), rhs, freeRegister)
        storeArrayElem(ident, exprList, freeRegister) 
      case _                            => 
    }
    restoreReg(freeRegister)
  }

  /*
  Translating an AssignRHS to ARM Language
  Returns a pair consisting of: 
      - Boolean (True if the size of RHS is a byte)
      - ListBuffer[Instr] (Translated instructions)
  */
  def transAssignRHS(t: Type, rhs: AssignRHS, freeRegister: Register): Boolean = {
    rhs match {
      case expr : Expr => 
        transExp(expr, freeRegister)
      case Fst(ident : Ident) => 
        transPairElem(ident, 1, freeRegister)
        getPairElem(ident, 1, freeRegister)
      case Snd(ident : Ident) => 
        transPairElem(ident, 2, freeRegister)
        getPairElem(ident, 2, freeRegister)
      case Call(ident, argList) =>
        transCall(ident, argList, freeRegister)
      case ArrayLiter(list) => 
        transArrayLiter(t, list, freeRegister)
      case NewPair(fst, snd) =>  
        transAssignRHSPair(t, fst, snd, freeRegister)
      case _ =>
    }
    isByte(t)
  }
}


