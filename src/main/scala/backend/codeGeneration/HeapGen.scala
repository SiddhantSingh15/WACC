package backend.codeGeneration

object HeapGen {

  private var heap = Map.empty[Ident, Int]
  private var addresses = Set.empty[Int]
  private var next = 0
  private val OVFLOW_RS = 31
  private val unallocMemErr = "MemErr: invalid, memory not allocated."
  private val doubleFreeErr = "MemErr: invalid, double free."

  def transHeap(tpe: Type, allocType: Heap, reg: Register): ListBuffer[Instr] = {
    val instructions = ListBuffer.empty[Instr]

    val PointerType(in) = tpe

    allocType match {
      case Calloc(num, size, _) =>
        instructions ++= transExp(size, reg)
        instructions += Mov(R1, reg)
        instructions ++= transExp(num, reg)
        instructions += Mov(resultRegister, reg)
        instructions += Bl(Label("calloc"))
      
      case Malloc(size, _) =>
        instructions ++= transExp(size, reg)
        instructions += Mov(resultRegister, reg)
        instuctionms += Bl(Label("malloc"))

      case Realloc(ptr, size, _) =>
        instructions ++= transExp(size, reg)
        instructions += Mov(R1, reg)
        instructions ++= transExp(ptr, reg)
        instructions += Mov(resultRegister, reg)
        instructions += Bl(Label("realloc"))
    }

    instructions += Mov(reg, resultRegister)
    instructions
  }
}
