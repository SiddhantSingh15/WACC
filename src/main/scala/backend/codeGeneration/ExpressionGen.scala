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
                instructions += Ldr(rd, Load_Mem(number))
            case bool: BoolLiter =>
                instructions += Mov(rd, Load_Int(boolToInt(bool)))
            case CharLiter(character) =>
                instructions += Mov(rd, Load_Char(character))
            case StrLiter(string) => // requires use of data table
            case PairLiter() => instructions += Ldr(rd, Load_Mem(0)) // TODO: remove magic number
            case Ident(id) =>
            case ArrayElem(id, exprs) =>
            case unOp: UnOp =>
            case binOp: BinOp =>
            case _ =>

        }

        instructions

    }
  
}
