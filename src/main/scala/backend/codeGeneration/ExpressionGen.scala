package backend.codeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._

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
                val instrs = ListBuffer[Instr](Ldr(rd, Load_Mem(number)))
                instructions ++= instrs
            case bool: BoolLiter =>
                val instrs = ListBuffer[Instr](Ldr(rd, Load_Int(boolToInt(bool))))
                instructions ++= instrs
            case CharLiter(character) =>
            case StrLiter(string) =>
            case PairLiter() =>
            case Ident(id) =>
            case ArrayElem(id, exprs) =>
            case unOp: UnOp =>
            case binOp: BinOp =>
            case _ =>

        }

        instructions

    }
  
}
