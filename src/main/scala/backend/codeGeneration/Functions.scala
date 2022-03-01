package backend.codeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._
import backend.CodeGen._
import frontend.SymbolTable
import backend.codeGeneration.ExpressionGen._
import backend.codeGeneration.PairsGen._
import scala.collection.mutable.ListBuffer


object Functions {

    def transCall(ident : Ident, arguments : Option[ArgList], freeRegister : Register): ListBuffer[Instr] = {
      val (instrs, toInc) = storeArguments(arguments, freeRegister)
      instrs += Bl(Label("f_" + ident))
      instrs ++= incrementSP(toInc)
      instrs += Mov(freeRegister, resultRegister)
      instrs
    }

    def transFunction (f : Func) : Unit = {
      val Func(tpe, id, paramList, stats) = f 
      currLabel = Label("f_" + id)
      val prevScopeSP = SP_scope
      symbTable = symbTable.nextScope

      translateFuncParams(paramList)

      SP_scope = SP_curr
      
      val instructions = ListBuffer.empty[Instr]

      for (stat <- stats) {
        instructions ++= transStat(stat, ListBuffer(Push(ListBuffer(R14_LR))))
      }

      SP_scope = prevScopeSP
      symbTable = symbTable.prev
      userTable.add(currLabel, instructions)
    }

    private def storeArguments(arguments : Option[ArgList] = None, register : Register): (ListBuffer[Instr], Int) = {
      var instructions = ListBuffer.empty[Instr]
      var offset = 0
      arguments match {
        case Some(ArgList(args)) => 
          for (a <- args.reverse) {
            val tpe = typeConverter(a)
            instructions ++= transExp(a, register)
            instructions += StrOffsetIndex(isByte(tpe), register, R13_SP, -getTypeSize(tpe))
           SP_curr += getTypeSize(tpe)
            offset += getTypeSize(tpe)
          }
        case None                =>
      }
     SP_curr -= offset
      (instructions, offset)
    }
    
    private def translateFuncParams(params : ParamList) : Unit = {
      params match {
        case ParamList(pList) =>
          var curr = SIZE_ADDR
          var prev = 0
          for(param <- pList) {
              val Param(tpe, id) = param 
            SP_curr += prev
              prev = getTypeSize(tpe)
              symbTable.add(id, -curr, tpe)
          }
      }
    }
    

    def transReturn(expr : Expr): ListBuffer[Instr] = {
        val register = saveReg()
        val instrs = transExp(expr, register)
        instrs += Mov(resultRegister, register)
        if(SP_scope > 0) { 
            instrs ++= addSP(SP_scope)
        }

        instrs ++= ListBuffer(
            Pop(ListBuffer(R15_PC)),
            Pop(ListBuffer(R15_PC)),
            Ltorg
        )
        addFreeReg(register)
        instrs
    }
}
