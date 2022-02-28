package backend.codeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._
import backend.CodeGen._
import frontend.SymbolTable

import scala.collection.mutable.ListBuffer

object ExpressionGen {

    val INT_TRUE = 1
    val INT_FALSE = 0

    def boolToInt(bool: BoolLiter): Int = {
        bool match {
            case True => INT_TRUE
            case False => INT_FALSE
        }
    }

    def transExp(expr: Expr, rd: Register): ListBuffer[Instr] = {
        val instructions = ListBuffer.empty[Instr]

        expr match {
            case IntLiter(number) =>
                instructions += Ldr(rd, Load_Mem(number))

            case bool: BoolLiter =>
                instructions += Mov(rd, Imm_Int(boolToInt(bool)))

            case CharLiter(character) =>
                instructions += Mov(rd, Imm_Char(character))

            case StrLiter(string) => 
                val label = dataTable.addData(string)
                instructions ++= ListBuffer(Ldr(rd, DataLabel(label)))

            case PairLiter() => 
                instructions += Ldr(rd, Load_Mem(0)) // TODO: remove magic number

            case Ident(id) =>
            case ArrayElem(id, exprs) =>
            case unOp: UnOp =>
                instructions ++= transUnOp(unOp, rd)

            case binOp: BinOp =>
            case _ =>

        }

        instructions

    }

    def transUnOp(op: UnOp, rd: Register): ListBuffer[Instr] = {
        op match {
            case Not(expr) =>
                transExp(expr, rd) += Eor(rd, rd, Imm_Int(INT_TRUE))
            case Negation(expr) =>
                transExp(expr, rd) ++= ListBuffer(
                    RsbS(rd, rd, Imm_Int(0)) // remove magic number
                 // , BranchLinkCond(OF, RunTimeRror) TODO: runtime errors
                )
            case Len(ident: Ident) =>
                val (i, t) = symbTable(ident)
                ListBuffer(
                    Ldr(rd, RegisterOffset(R13_SP, stackPointer - i)),
                    Ldr(rd, RegAdd(rd))
                )
                
            case Ord(expr) =>
                transExp(expr, rd)
            case Chr(expr) =>
                transExp(expr, rd)
            case _  =>
                ListBuffer.empty[Instr]
        }
        ListBuffer.empty[Instr]
    }
  
}
