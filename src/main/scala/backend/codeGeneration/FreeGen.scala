package backend.codeGeneration

import scala.collection.mutable.ListBuffer
import frontend.AST
import backend.Opcodes.Instr
import backend.CodeGen._
import frontend.SymbolTable
import frontend.AST.{Expr, Ident, Pair, ArrayType}
import backend.Operand
import backend.Opcodes.{Ldr, Mov, Bl}
import backend.Operand.{R13_SP}
import backend.DefinedFuncs.RuntimeErrors.addRTE
import backend.DefinedFuncs.PreDefinedFuncs.{FreeArray, FreePair}

object FreeGen {

  /*Translating a Free statement into ARM language*/
  def transFree(expr: Expr): ListBuffer[Instr] = {
    val instructions = ListBuffer.empty[Instr]
    expr match {
      case id: Ident => 
        val freeRegister = saveReg()
        val (i, t) = symbTable(id)

        instructions += Ldr(freeRegister, R13_SP, stackPointer - i)

        /*Have to move to R0 to call free function*/
        instructions += Mov(resultRegister, freeRegister)

        restoreReg(freeRegister)

        t match {
          case _: Pair      => instructions += Bl(addRTE(FreePair))
          case _: ArrayType => instructions += Bl(addRTE(FreeArray))
          case _            => 
        }

      case _         =>
    }
    instructions
  }
}
