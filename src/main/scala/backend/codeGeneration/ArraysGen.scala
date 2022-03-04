package backend.CodeGeneration

import frontend.AST._
import backend.Operand._
import backend.Opcodes._
import scala.collection.mutable.ListBuffer
import backend.CodeGen._
import backend.CodeGeneration.ExpressionGen._
import backend.DefinedFuncs.RuntimeErrors._
import backend.DefinedFuncs.PreDefinedFuncs._

object ArraysGen {

  /*Getting type of entires in Array */
  def getInnerType(arrayT: Type): Type = 
    arrayT match {
      case ArrayType(innerType) => innerType
      case _                    => ???
    }

  /*Loading an ArrayElem into a given Register*/
  def loadArrayElem(ident: Ident, exprs: List[Expr], rd: Register): Unit = {
    val isByte = transArrayElem(ident, exprs, rd)
    currInstructions += Ldr(isByte, rd, rd, 0)
  }

  /*Storing an Expr from a Register into the ArrayElem*/
  def storeArrayElem(ident: Ident, exprs: List[Expr], rd: Register): Unit = {
    val freeReg = saveReg()
    val isByte = transArrayElem(ident, exprs, freeReg)
    currInstructions += Str(isByte, rd, freeReg, 0)
    restoreReg(freeReg)
  }

  /*Translating an ArrayElem to the ARM language*/
  def transArrayElem(ident: Ident, exprs: List[Expr], rd: Register): Boolean = {
    var (i, t) = symbTable(ident)
    val typeSize = getTypeSize(t)
    val spOffset = stackPointer - i

    currInstructions += Add(rd, R13_SP, Imm_Int(spOffset))
    val nextReg = saveReg()

    for (expr <- exprs) {
      t = getInnerType(t)
      transExp(expr, nextReg)
      currInstructions += Ldr(rd, RegAdd(rd))
      currInstructions += Mov(resultRegister, nextReg)
      currInstructions += Mov(R1, rd)
      currInstructions += Bl(addRTE(ArrayBounds))
      /*accounting for array size*/
      currInstructions += Add(rd, rd, Imm_Int(SIZE_INT))
      
      if (isByte(t)) {
        currInstructions += Add(rd, rd, nextReg)
      } else {
        /*there is a 4 byte difference between elements*/
        currInstructions += Add(rd, rd, LSL(nextReg, Imm_Int(2)))
      }
    }
    restoreReg(nextReg)
    isByte(t)
  }


  /*Translating an ArrayLiter into the ARM language*/
  def transArrayLiter(t: Type, arr: List[Expr], freeReg: Register): Unit = {
    val ArrayType(innerT) = t
    val size = arr.size

    /*Array size + (size of each indvidual elem) * (# of elems)*/
    currInstructions += Ldr(
      resultRegister,
      Load_Mem(SIZE_INT + size * getTypeSize(innerT))
    )
    currInstructions += Bl(Label("malloc"))
    currInstructions += Mov(freeReg, resultRegister)

    val nextFreeReg = saveReg()
    val isbyte = isByte(innerT)

    /*Strogin all elements in memory*/
    for (i <- 0 until size) {
      transExp(arr(i), nextFreeReg)
      if (isbyte) {
        currInstructions += Str(isbyte, nextFreeReg, freeReg, i + SIZE_INT)
      } else {
        currInstructions += Str(isbyte, nextFreeReg, freeReg, (i + 1) * SIZE_ADDR)
      }
    }

    /*Storing size of array in memory*/
    currInstructions += Ldr(nextFreeReg, Load_Mem(size))
    currInstructions += Str(nextFreeReg, RegAdd(freeReg))
    restoreReg(nextFreeReg)
  }
}
