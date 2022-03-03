package backend.codeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._
import backend.CodeGen._
import frontend.SymbolTable
import backend.codeGeneration.ExpressionGen._
import backend.codeGeneration.PairsGen._
import backend.codeGeneration.Functions._
import backend.codeGeneration.ArraysGen._

import scala.collection.mutable.ListBuffer


object Assignments {

    /*Translating declaration of new variable to ARM language*/
    def translateDeclaration(
        t: Type,
        id : Ident,
        rhs : AssignRHS
    ): ListBuffer[Instr] = {
        val instrs = ListBuffer.empty[Instr]
        SP_scope += getTypeSize(t)
        symbTable.add(id, SP_scope, t)
        val spOffset = stackPointer - SP_scope
        val freeRegister = saveReg()
        val (isByte, newInstrs) = transAssignRHS(t, rhs, freeRegister)
        instrs ++= newInstrs
        instrs += Str(isByte, freeRegister, R13_SP, spOffset)
        restoreReg(freeRegister)
        instrs
    }

    /*Translating assignment of a variable to ARM language*/
    def transAssignment(
        lhs: AssignLHS,
        rhs: AssignRHS
    ): ListBuffer[Instr] = {
        val instructions = ListBuffer.empty[Instr]
        val freeRegister = saveReg()

        lhs match {
            case id : Ident =>
                val (index, t) = symbTable(id)
                val (isByte, newInstrs) = transAssignRHS(t, rhs, freeRegister)
                instructions ++= newInstrs
                val spOffset = stackPointer - index  
                instructions += Str(isByte, freeRegister, R13_SP, spOffset)
            case Fst(id : Ident) => 
                instructions ++= transPairAssign(rhs, id, 1, freeRegister)
            case Snd(id : Ident) => 
                instructions ++= transPairAssign(rhs, id, 2, freeRegister)
            case x@ArrayElem(ident, exprList) =>
                val (_, newInstrs) = transAssignRHS(getExprType(x), rhs, freeRegister)
                instructions ++= newInstrs
                instructions ++= storeArrayElem(ident, exprList, freeRegister) 
            case _ => ???
        }
        restoreReg(freeRegister)
        instructions
    }

    /*
    Translating an AssignRHS to ARM Language
    Returns a pair consisting of: 
        - Boolean (True if the size of RHS is a byte)
        - ListBuffer[Instr] (Translated instructions)
    */
    def transAssignRHS(
        t: Type,
        rhs: AssignRHS,
        freeRegister: Register
    ): (Boolean, ListBuffer[Instr]) = {
        val instrs = ListBuffer.empty[Instr]
        rhs match {
            case expr : Expr => 
                instrs ++= transExp(expr, freeRegister)
            case Fst(ident : Ident) => 
                instrs ++= transPairElem(ident, 1, freeRegister)
                instrs += getPairElem(ident, 1, freeRegister)
            case Snd(ident : Ident) => 
                instrs ++= transPairElem(ident, 2, freeRegister)
                instrs += getPairElem(ident, 2, freeRegister)
            case Call(ident, argList) =>
                instrs ++= transCall(ident, argList, freeRegister)
            case ArrayLiter(list) => 
                instrs ++= transArrayLiter(t, list, freeRegister)
            case NewPair(fst, snd) =>  
                instrs ++= transAssignRHSPair(t, fst, snd, freeRegister)
            case _ =>
        }
        (isByte(t), instrs)
    }
}


