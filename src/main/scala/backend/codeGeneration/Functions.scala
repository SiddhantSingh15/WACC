package backend.codeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._
import backend.CodeGen._
import frontend.SymbolTable
import backend.codeGeneration.ExpressionGen._
import backend.codeGeneration.PairsGen._


object Functions {

    def translateCall(id : Ident, 
                      arguments : Option[ArgList], 
                      freeRegister : Register): ListBuffer[Instr] = {

                        val (instrs, toInc) = storeArguments(arguments, freeRegister)
                        instrs += Bl(Label("f_" + id))
                        instrs ++= incrementSP(toInc)
                        instrs += Mov(freeRegister, resultRegister)
                        instrs

                      }
    private def storeArguments(arguments : Option[ArgList] = None,
                               register : Register): (ListBuffer[Instr], Int) = {
                                   null
                               }
    
    private def translateFuncParams(params : Option[ParamList]) : Unit = {
        params match {
            case None =>
            case Some(ParamList(paramList)) =>
                var curr = ADDRESS_SIZE
                var prev = 0
                for(param <- paramList) {
                    val Param(tpe, id) = param 

                    //???
                    curr += prevSize

                    prevSize = getTypeSize(tpe)
                    symbTable.add(id, -curr, tpe)
                }
        }
    }

    //case class Func(tpe : Type, ident : Ident, paramList : ParamList, stats : List[Stat])
    
    //make sure to understand translateFunction before writing it
    def translateFunction(f : Func) : Unit = {
        val Func(tpe, id, paramList, stats) = f 
        currLabel = Label("f_" + id)
        val prevScopeSP = scopeSP
        symbTable = symbTable.nextScope
        
    }


    def translateReturn(expr : Expr): ListBuffer[Instr] = {
        val register = saveReg()
        val instrs = transExp(expr, register)
        instrs += Mov(resultRegister, register)
        if(scopeSP > 0) { 
            instrs ++= addSP(scopeSP)
        }

        instrs ++= ListBuffer(
            Pop(ListBuffer(PC)),
            Pop(ListBuffer(PC)),
            Ltorg
        )
        
        restoreReg(register)
        instrs
    }
}
