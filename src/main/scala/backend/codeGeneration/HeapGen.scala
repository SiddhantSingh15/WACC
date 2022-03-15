package backend.CodeGeneration

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map
import scala.collection.immutable.Set

import frontend.AST._
import backend.Operand._
import backend.Opcodes._
import backend.CodeGen._
import backend.CodeGeneration.ExpressionGen.{transExp}
import backend.DefinedFuncs.PreDefinedFuncs._
import backend.Condition._
import backend.DefinedFuncs.RuntimeErrors._
import backend.CodeGeneration.CodeGenHelper._

object HeapGen {

  private var heap = Map.empty[Ident, Int]
  private var addresses = Set.empty[Int]
  private var next = 0
  private val OVFLOW_RS = 31
  private val unallocMemErr = "MemErr: invalid, memory not allocated."
  private val doubleFreeErr = "MemErr: invalid, double free."


  def populateHeap(ident: Ident, rhs: AssignRHS): Unit = 
    rhs match {
      case id: Ident =>
        heap += ((ident, heap(id)))
      case h: Heap   =>
        val nxt = incNext
        heap += ((ident, nxt))
        addresses += nxt
      case _         => 
  }

  private def incNext: Int = {
    next += 1
    next
  }

  def transPointer(tpe: Type, elem: Expr, rd: Register, otherReg: Register) = {
    currInstructions.add(Ldr(rd, RegAdd(rd)))
    transExp(elem, otherReg)
    ptrArith(tpe, otherReg)
    currInstructions.add(Add(rd, rd, otherReg))
  }

  def freePointer(ident: Ident) =  {
    val address = heap(ident)
    
    if (addresses(address)) {
      addresses -= address
      currInstructions.add(Bl(Label("free")))
    } else {
      freeErr(doubleFreeErr)
    }
  }

  private def freeErr(output: String) = {
    val label = dataTable.addData(output)
    currInstructions.add(Ldr(resultRegister, DataLabel(label)))
    currInstructions.add(Bl(addRTE(RuntimeError)))
  }

  def throwBadFreeErr = {
    freeErr(unallocMemErr)
  }

  def transHeap(tpe: Type, allocType: Heap, reg: Register): Unit = {
    val PointerType(in) = tpe

    allocType match {
      case Calloc(num, size)  =>
        transExp(size, reg)
        currInstructions.add(Mov(R1, reg))
        transExp(num, reg)
        currInstructions.add(Mov(resultRegister, reg))
        currInstructions.add(Bl(Label("calloc")))
      
      case Malloc(size)       =>
        transExp(size, reg)
        currInstructions.add(Mov(resultRegister, reg))
        currInstructions.add(Bl(Label("malloc")))

      case Realloc(ptr, size) =>
        transExp(size, reg)
        currInstructions.add(Mov(R1, reg))
        transExp(ptr, reg)
        currInstructions.add(Mov(resultRegister, reg))
        currInstructions.add(Bl(Label("realloc")))
    }

    currInstructions.add(Mov(reg, resultRegister))
  }

  def addressManip(lhs: Expr, rd: Register): Unit = {
    getExprType(lhs) match {
      case ptr: PointerType => ptrArith(ptr, rd)
      case _                => ???
    }
  }

  private def ptrArith(tpe: Type, rd: Register) = {
    val PointerType(t) = tpe
    val size = getTypeSize(t)
    val rm = saveReg()
    if (size > SIZE_CHAR) {
      currInstructions.add(Ldr(rm, Imm_Int(size)))
      currInstructions.add(SMul(rd, rm, rd, rm))
      restoreReg(rm)
      currInstructions.add(BranchLinkCond(NE, addRTE(Overflow)))
    }
  }
}
