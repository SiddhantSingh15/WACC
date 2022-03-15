package backend.codeGeneration

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.Map
import scala.collection.immutable.Set

import frontend.AST._
import backend.Operand._
import backend.Opcodes._
import backend.CodeGen._
import backend.CodeGeneration.ExpressionGen.{transExp}


object HeapGen {

  private var heap = Map.empty[Ident, Int]
  private var addresses = Set.empty[Int]
  private var next = 0
  private val OVFLOW_RS = 31
  private val unallocMemErr = "MemErr: invalid, memory not allocated."
  private val doubleFreeErr = "MemErr: invalid, double free."

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
}
