package backend.CodeGeneration

import scala.collection.mutable.ListBuffer
import frontend.AST
import backend.Opcodes.Instr
import backend.CodeGen._
import frontend.SymbolTable
import frontend.AST.{Expr, Ident, Pair, ArrayType, PointerType}
import backend.Operand
import backend.Opcodes.{Ldr, Mov, Bl}
import backend.Operand.{R13_SP}
import backend.DefinedFuncs.RuntimeErrors.addRTE
import backend.DefinedFuncs.PreDefinedFuncs.{FreeArray, FreePair}
import backend.CodeGeneration.HeapGen._

object FreeGen {

  /*Translating a Free statement into ARM language*/
  def transFree(expr: Expr): Unit = {

    expr match {
      case id: Ident => 
        val freeRegister = saveReg()
        val (i, t) = symbTable(id)

        currInstructions.addAll(ListBuffer[Instr](
          Ldr(freeRegister, R13_SP, currSP - i),
          /*Have to move to R0 to call free function*/
          Mov(resultRegister, freeRegister)
        ))

        restoreReg(freeRegister)

        t match {
          case _: Pair        => currInstructions.add(Bl(addRTE(FreePair)))
          case _: ArrayType   => currInstructions.add(Bl(addRTE(FreeArray)))
          case _: PointerType => freePointer(id)
          case _              => throwBadFreeErr
        }

      case _         =>
    }
  }
}
