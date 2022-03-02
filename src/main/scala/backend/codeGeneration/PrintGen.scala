package backend.codeGeneration

import backend.CodeGen._
import frontend.AST._
import backend.codeGeneration.ExpressionGen._
import backend.DefinedFuncs.PreDefinedFuncs._
import backend.DefinedFuncs.PrintInstrs._
import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import backend.tableDataTypes.functionTable

object PrintGen {
  

    def transPrint(
        expr: Expr,
        isLine: Boolean
    ): ListBuffer[Instr] = {
        val t = getExprType(expr)
        val freeReg = saveReg()
        val instrs = transExp(expr, freeReg)
        instrs += Mov(resultRegister, freeReg)

        var printFunc: PreDefFunc = null
        t match {
            case CharType => 
                printFunc = PutChar
            case Int => 
                printFunc = PrintInt
            case Bool =>
                printFunc = PrintBool
            case String | ArrayType(CharType) =>
                printFunc = PrintString
            case 
            case _ => 
        }
        
        for (i <- 0 until printFunc.msgs.length) {
            dataTable.addLabel(
                printFunc.msgName(i),
                printFunc.msgs(i)
            )
        }
        instrs += Bl(printFunc.functionLabel)
        if (printFunc.function == null) { //TODO: remove usage of null
            funcTable.addFunction(printFunc.function)
        }  

        if (isLine) {
            instrs += Bl(PrintLn.functionLabel)
            dataTable.addLabel(
                PrintLn.msgName(0),
                PrintLn.msgs(0)
            )
            funcTable.addFunction(PrintLn.function)
        }
        restoreReg(freeReg)
        instrs
    }
}
