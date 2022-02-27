package backend

import backend.Operand._
import backend.Condition._

object InstructionSet {
    sealed trait Instruction

    case class Add(rd: Register, rn: Register, op2: Operand) extends Instruction {
        override def toString: String = "ADD " + rd + ", " + rn + ", " + op2
    }
    case class AddS(rd: Register, rn: Register, op2: Operand) extends Instruction {
        override def toString: String = "ADDS " + rd + ", " + rn + ", " + op2
    }
    case class Sub(rd: Register, rn: Register, op2: Operand) extends Instruction {
        override def toString: String = "SUB " + rd + ", " + rn + ", " + op2
    }
    case class SubS(rd: Register, rn: Register, op2: Operand) extends Instruction {
        override def toString: String = "SUBS " + rd + ", " + rn + ", " + op2
    }
    case class SMul(rdLo: Register, rdHi: Register, rn: Register, rm: Register) extends Instruction {
        override def toString: String =
        "SMULL " + rdLo + ", " + rdHi + ", " + rn + ", " + rm
    }
    case class RsbS(rd: Register, rn: Register, op2: Operand) extends Instruction {
        override def toString: String = "RSBS " + rd + ", " + rn + ", " + op2
    }

    /* Comparison */
    case class Cmp(rn: Register, op2: Operand) extends Instruction {
        override def toString: String = "CMP " + rn + ", " + op2
    }
}