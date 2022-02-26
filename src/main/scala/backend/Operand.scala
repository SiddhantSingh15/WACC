package backend

object Operand {
    sealed trait Operand

    sealed case class ImmMem(n: Int) extends Operand {
        override def toString: String = s"=$n"
    }


    sealed trait Register extends Operand
    case object R0 extends Register
    case object R1 extends Register
    case object R2 extends Register
    case object R3 extends Register
    case object R4 extends Register
    case object R5 extends Register
    case object R6 extends Register
    case object R7 extends Register
    case object R8 extends Register
    case object R9 extends Register
    case object R10 extends Register
    case object R11 extends Register
    case object R12 extends Register
    case object R13_SP extends Register
    case object R14_LR extends Register {
        override def toString: String = "lr"
    }
    case object R15_PC extends Register {
        override def toString: String = "pc"
    }

    sealed case class Load_Imm(value: Int) extends Operand

    sealed case class Load_Mem(value: Int) extends Operand
}