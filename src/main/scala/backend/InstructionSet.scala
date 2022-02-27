package backend

import scala.collection.mutable.ListBuffer
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

    /* Logical Operations */
    case class And(rd: Register, rn: Register, op2: Operand) extends Instruction {
    override def toString: String = "AND " + rd + ", " + rn + ", " + op2
    }
    case class Or(rd: Register, rn: Register, op2: Operand) extends Instruction {
        override def toString: String = "ORR " + rd + ", " + rn + ", " + op2
    }
    case class Eor(rd: Register, rn: Register, op2: Operand) extends Instruction {
        override def toString: String = "EOR " + rd + ", " + rn + ", " + op2
    }
    /* Branching */
    case class Branch(label: Label) extends Instruction {
        override def toString: String = "B " + label
    }
    // Branch {Condition}
    case class BranchCond(cond: Condition, label: Label) extends Instruction {
        override def toString: String = "B" + cond + " " + label
    }
    // Branch Link
    case class BranchLink(label: Label) extends Instruction {
        override def toString: String = "BL " + label
    }
    // Branch Link {Condition}
    case class BranchLinkCond(cond: Condition, label: Label) extends Instruction {
        override def toString: String = "BL" + cond + " " + label
    }

    /* Stack Operations */
    case class Push(rs: ListBuffer[Register]) extends Instruction {
        override def toString: String = "PUSH " + "{" + rs.mkString(", ") + "}"
    }
    case class Pop(rs: ListBuffer[Register]) extends Instruction {
        override def toString: String = "POP " + "{" + rs.mkString(", ") + "}"
    }

    case class Label(s: String) {
        override def toString: String = s
    }
    case class Data(label: Label, s: String)
}