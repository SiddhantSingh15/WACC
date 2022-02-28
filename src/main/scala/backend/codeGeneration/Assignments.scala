package backend.codeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._
import backend.CodeGen._
import frontend.SymbolTable
import backend.codeGeneration.ExpressionGen._

import scala.collection.mutable.ListBuffer


object Assignments {

    def translateDeclaration(
        t: Type,
        id : Ident,
        rhs : AssignRHS
    ): ListBuffer[Instr] = {
        val instrs = ListBuffer.empty[Instr]
        scopeSP += getTypeSize(t)
        symbTable.add(id, scopeSP, t)
        val spOffset = currSP - scopeSP
        val freeRegister = getFreeRegister()
        val (isByte, newInstrs) = translateAssignRHS(t, rhs, freeRegister)
        instrs ++= newInstrs
        instrs += Str(isByte, freeRegister, R13_SP, spOffset)
        addBackRegister(freeRegister)
        instrs
    }



    def translateAssignment(
        lhs: AssignLHS,
        rhs: AssignRHS
    ): ListBuffer[Instr] = {
        val instrs = ListBuffer.empty[Instr]
        val freeRegister = getFreeRegister()

        lhs match {
            case id : Ident =>
                val (index, t) = symbTable(id)
                val (isByte, newInstrs) = translateAssignRHS(t, rhs, freeRegister)
                instrs ++= newInstrs
                val spOffset = currSP - index  
                instrs += Str(isByte, freeRegister, R13_SP, spOffset)
            case Fst(id : Ident) => null //TODO
            case Snd(id : Ident) => null //TODO
            case ArrayElem(ident, exprList) => null //TODO
            case _ => null  
        }
        addBackRegister(freeRegister)
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

            case Fst(id) => null //TODO
            case Snd(id) => null //TODO
            case Call(ident, argList) => null //TODO
            case ArrayLiter(list) => null //TODO
            case NewPair(exprOne, exprTwo) =>  null //TODO 
        }
        (isByte(t), instrs)
    }
}


