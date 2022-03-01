package backend.codeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._
import backend.CodeGen._
import frontend.SymbolTable
import backend.codeGeneration.ExpressionGen._
import backend.codeGeneration.PairsGen._

import scala.collection.mutable.ListBuffer


object Assignments {

    def translateDeclaration(
        t: Type,
        id : Ident,
        rhs : AssignRHS
    ): ListBuffer[Instr] = {
        val instrs = ListBuffer.empty[Instr]
        SP_scope += getTypeSize(t)
        symbTable.add(id, SP_scope, t)
        val spOffset =SP_curr - SP_scope
        val freeRegister = saveReg()
        val (isByte, newInstrs) = translateAssignRHS(t, rhs, freeRegister)
        instrs ++= newInstrs
        instrs += Str(isByte, freeRegister, R13_SP, spOffset)
        restoreReg(freeRegister)
        instrs
    }



    def translateAssignment(
        lhs: AssignLHS,
        rhs: AssignRHS
    ): ListBuffer[Instr] = {
        val instrs = ListBuffer.empty[Instr]
        val freeRegister = saveReg()

        lhs match {
            case id : Ident =>
                val (index, t) = symbTable(id)
                val (isByte, newInstrs) = translateAssignRHS(t, rhs, freeRegister)
                instrs ++= newInstrs
                val spOffset =SP_curr - index  
                instrs += Str(isByte, freeRegister, R13_SP, spOffset)
            case Fst(id : Ident) => 
                instrs ++= transPairAssign(rhs, id, 0, freeRegister)
            case Snd(id : Ident) => 
                instrs ++= transPairAssign(rhs, id, 1, freeRegister)
            case x@ArrayElem(ident, exprList) => null //TODO
            case _ => null  
        }
        restoreReg(freeRegister)
        instrs
    }

    def translateAssignRHS(
        t: Type,
        rhs: AssignRHS,
        freeRegister: Register
    ): (Boolean, ListBuffer[Instr]) = {
        val instrs = ListBuffer.empty[Instr]
        rhs match {
            case expr : Expr => instrs ++= transExp(expr, freeRegister)

            case Fst(id : Ident) => 
                instrs ++= transPairElem(id, 0, freeRegister)
            case Snd(id : Ident) => 
                instrs ++= transPairElem(id, 1, freeRegister)
            case Call(ident, argList) =>
                // instrs ++= translateCall(ident, argList, freeRegister)
            case ArrayLiter(list) => 
                // instrs ++= translateArrayLiter(t, list, freeRegister)
            case NewPair(fst, snd) =>  
                // instrs ++= assignRHSPair(t, fst, snd, freeRegister)
            case _ =>
        }
        (isByte(t), instrs)
    }
}


