package backend.codeGeneration

object HeapGen {
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
        
    }
  }
}
