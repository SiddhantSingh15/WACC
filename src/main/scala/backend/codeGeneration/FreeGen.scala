package backend.codeGeneration

import scala.collection.mutable.ListBuffer
import frontend.AST
import backend.Opcodes.Instr
import backend.CodeGen._
import frontend.SymbolTable
import frontend.AST.{Expr, Ident}
import backend.Operand
import backend.Opcodes.{Ldr, Mov}
import backend.Operand.{R13_SP}

object FreeGen {

  def transFree(expr: Expr): ListBuffer[Instr] = {
    val instructions = ListBuffer.empty[Instr]
    expr match {
      case id: Ident => 
        val freeRegister = freeRegs()
        val (i, t) = symbTable(id)

        instructions += Ldr(freeRegister, R13_SP)
        instructions += Mov(resultRegister, freeRegister)

        addReg(freeRegister)

      case _ => 
    }
    instructions
  }
}
