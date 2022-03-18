package backend.CodeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._
import backend.CodeGen._
import frontend.SymbolTable
import backend.CodeGeneration.ExpressionGen._
import backend.CodeGeneration.PairsGen._
import backend.CodeGeneration.CodeGenHelper._
import scala.collection.mutable.ListBuffer


object Functions {

  /* Translates the CALL functions.
   * Takes an Ident, ArgList and a free register.
   * Returns a list of instructions:
   *   Bl(label) + label returned by incrementing stack pointer + Mov(freeReg, resReg)
   */
  def transCall(ident : Ident, arguments : ArgList, freeRegister : Register): Unit = {
    val toInc = storeArguments(arguments, freeRegister)
    currInstructions.add(Bl(Label("f_" + ident)))
    incrementSP(toInc)
    currInstructions.add(Mov(freeRegister, resultRegister))
  }


  /*
   * Translates FUNC functions.
   * Takes a function.
   * Updates the scope.
   * Translates the parameters of the function call. 
   * Returns the list of instructions generated.
   */
  def transFunction (f : Func) : Unit = {
    val Func(tpe, id, paramList, stats) = f 
    currLabel = Label("f_" + id)
    val prevScopeSP = scopeSP
    symbTable = symbTable.getNextScope
    val maxSpDepth = symbTable.spMaxDepth(id)
    translateFuncParams(paramList)

    scopeSP = currSP
    currSP += maxSpDepth
    currInstructions.add(Push(ListBuffer(R14_LR)))
    decrementSP(maxSpDepth)

    for (stat <- stats) {
      transStat(stat)
    }

    if (maxSpDepth > 0) {
      currSP -= maxSpDepth
    }

    scopeSP = prevScopeSP
    symbTable = symbTable.prev
    currInstructions.addAll(ListBuffer(
      Pop(ListBuffer(R15_PC)),
      Ltorg
    ))
    funcTable.add(currLabel, currInstructions)
    currInstructions = BlockInstrs(ListBuffer.empty[Instr])
  }

  /*
   * Translates the arguments using transExp.
   * Increments the stack pointer and offset.
   * Returns the list of instructions along with the total offset.
   */
  private def storeArguments(arguments : ArgList, register : Register): Int = {
    var offset = 0
    val ArgList(args) = arguments
      
    for (a <- args.reverse) {
      val tpe = getExprType(a)
      transExp(a, register)
      currInstructions.add(StrOffsetIndex(isByte(tpe), register, R13_SP, -getTypeSize(tpe)))
      currSP += getTypeSize(tpe)
      offset += getTypeSize(tpe)
    }
        
    currSP -= offset
    offset
  }
  
  /*
   * Helper function for transFunction().
   * Translates each function parameter and adds it to the symbol table.
   */
  private def translateFuncParams(params : ParamList) : Unit = {
    val ParamList(pList) = params
    var curr = SIZE_ADDR
    var prev = 0
    for(param <- pList) {
      val Param(tpe, id) = param 
      curr += prev
      prev = getTypeSize(tpe)
      symbTable.add(id, -curr, tpe, None)
    }
  }
  
  /*
   * Translates the RETURN functions.
   * Takes in an Expr.
   * Returns the translated instructions list.
   */
  def transReturn(expr : Expr): Unit = {
    val register = saveReg()
    transExp(expr, register)
    currInstructions.add(Mov(resultRegister, register))
    if(currSP > 0) { 
        incrementSP(currSP)
    }
    currInstructions.add(Pop(ListBuffer(R15_PC)))
    restoreReg(register)
  }
}
