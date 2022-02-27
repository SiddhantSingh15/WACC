package backend.codeGeneration

import scala.collection.mutable.ListBuffer
import frontend.AST
import backend.Opcodes.Instr
import backend.CodeGen._
import frontend.SymbolTable

object FreeGen {

  def transFree(expr: AST.Expr): ListBuffer[Instr] = {
    val instructions = ListBuffer.empty[Instr]
    val freeRegister = freeRegs()

    val (i, t) = SymbolTable(expr)
  }
}
