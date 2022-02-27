package backend.codeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._

import scala.collection.mutable.ListBuffer

object ExpressionGen {

    def transExp(expr: Expr, rd: Register): ListBuffer[Instr] = {
        val instructions = ListBuffer.empty[Instr]

        expr match {
            case IntLiter(number) =>
            case bool: BoolLiter =>
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
