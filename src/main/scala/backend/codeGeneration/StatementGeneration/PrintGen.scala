package backend.CodeGeneration

import backend.CodeGen._
import frontend.AST._
import backend.CodeGeneration.ExpressionGen._
import backend.DefinedFuncs.PreDefinedFuncs._
import backend.DefinedFuncs.PrintInstrs._
import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import backend.CodeGeneration.CodeGenHelper._

object PrintGen {
  
  /* Translates the PRINT functions.
   * Takes an expression and a boolean flag representing isLine.
   * Returns a list of instructions matching the type of expression and restores registers.
   */
  def transPrint(expr: Expr, isLine: Boolean): Unit = {
    val t = getExprType(expr)
    val freeReg = saveReg()
    transExp(expr, freeReg)
    currInstructions.add(Mov(resultRegister, freeReg))

    // Chooses the print functions based on the expression type.
    var printFunc: PreDefFunc = null
    t match {
      case CharType                     => 
        printFunc = PutChar
      case Int                          => 
        printFunc = PrintInt
      case Bool                         =>
        printFunc = PrintBool
      case String | ArrayType(CharType) =>
        printFunc = PrintString
      case ArrayType(_) | Pair(_, _) | PointerType(_)   =>
        printFunc = PrintReference
      case _                            => 
        ???
    }
      
    for (i <- 0 until printFunc.msgs.length) {
      dataTable.addLabel(
          printFunc.msgName(i),
          printFunc.msgs(i)
      )
    }
    currInstructions.add(Bl(printFunc.functionLabel))
    if (printFunc.function.isDefined) {
      preDefFuncTable.addFunction(printFunc.function.get)
    }  

    if (isLine) {
      currInstructions.add(Bl(PrintLn.functionLabel))
      dataTable.addLabel(
        PrintLn.msgName(0),
        PrintLn.msgs(0)
      )
      preDefFuncTable.addFunction(PrintLn.function.get)
    }
    restoreReg(freeReg)
  }
}
