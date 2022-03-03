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

  /* Translates the CALL functions.
   * Takes an Ident, ArgList and a free register.
   * Returns a list of instructions:
   *   Bl(label) + label returned by incrementing stack pointer + Mov(freeReg, resReg)
   */
  def transCall(ident : Ident, arguments : ArgList, freeRegister : Register): ListBuffer[Instr] = {
    val (instrs, toInc) = storeArguments(arguments, freeRegister)
    instrs += Bl(Label("f_" + ident))
    instrs ++= incrementSP(toInc)
    instrs += Mov(freeRegister, resultRegister)
    instrs
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
    val prevScopeSP = SP_scope
    symbTable = symbTable.getNextScope
    val maxSpDepth = symbTable.spMaxDepth(id)
    translateFuncParams(paramList)

    SP_scope = stackPointer
    stackPointer += maxSpDepth
    var instructions = ListBuffer[Instr](Push(ListBuffer(R14_LR)))
    instructions ++= decrementSP(maxSpDepth)

    for (stat <- stats) {
      instructions = transStat(stat, instructions)
    }

    if (maxSpDepth > 0) {
      stackPointer -= maxSpDepth
    }

    SP_scope = prevScopeSP
    symbTable = symbTable.prev
    instructions ++= ListBuffer(
      Pop(ListBuffer(R15_PC)),
      Ltorg
    )
    funcTable.add(currLabel, instructions)
  }

  /*
   * Translates the arguments using transExp.
   * Increments the stack pointer and offset.
   * Returns the list of instructions along with the total offset.
   */
  private def storeArguments(arguments : ArgList, register : Register): (ListBuffer[Instr], Int) = {
    var instructions = ListBuffer.empty[Instr]
    var offset = 0
    val ArgList(args) = arguments
      
    for (a <- args.reverse) {
      val tpe = getExprType(a)
      instructions ++= transExp(a, register)
      instructions += StrOffsetIndex(isByte(tpe), register, R13_SP, -getTypeSize(tpe))
      stackPointer += getTypeSize(tpe)
      offset += getTypeSize(tpe)
    }
        
    stackPointer -= offset
    (instructions, offset)
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
      symbTable.add(id, -curr, tpe)
    }
  }
  
  /*
   * Translates the RETURN functions.
   * Takes in an Expr.
   * Returns the translated instructions list.
   */
  def transReturn(expr : Expr): ListBuffer[Instr] = {
    val register = saveReg()
    val instrs = transExp(expr, register)
    instrs += Mov(resultRegister, register)
    if(stackPointer > 0) { 
        instrs ++= incrementSP(stackPointer)
    }

    instrs ++= ListBuffer(
      Pop(ListBuffer(R15_PC))
    )        
    restoreReg(register)
    instrs
  }
}
