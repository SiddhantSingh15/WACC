package backend

import backend.Operand
import scala.collection.mutable.ListBuffer

object Opcodes {
    sealed trait Opcode 

    // ARITHMETIC

    case class Add(rd: Register, rn: Register, op2: Operand) extends Opcode
    case class Sub(rd: Register, rn: Register, op2: Operand) extends Opcode
    case class Mul(rd: Register, rm: Register, rs: Register) extends Opcode

    // COMPARE

    case class Cmp(rn: Register, op2: Operand) extends Opcode

    // BRANCH

    case class B(label: String) extends Opcode
    case class Bl(label: String) extends Opcode

    // LOGICAL

    case class Mov(rd: Register, op2: Operand) extends Opcode
    case class And(rd: Register, rn: Register, op2: Operand) extends Opcode
    case class Xor(rd: Register, rn: Register, op2: Operand) extends Opcode
    case class Or(rd: Register, rn: Register, op2: Operand) extends Opcode

    // PUSH/ POP
    case class Push(regList: ListBuffer[Register]) extends Opcode
    case class Pop(regList: ListBuffer[Register]) extends Opcode
}
