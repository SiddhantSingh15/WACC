package backend

import backend.Operand._
import backend.Condition._
import scala.collection.mutable.ListBuffer

object Opcodes {

  case class BlockInstrs(instrBlock: ListBuffer[Instr]) {

    def add(instr: Instr): Unit = {
      instrBlock += instr
    }

    def addAll(instrs: ListBuffer[Instr]): Unit = {
      instrBlock ++= instrs
    }

    override def toString: String = {
      val sb = new StringBuilder
      instrBlock.foreach((i: Instr) => sb ++= s"\t\t$i\n")
      sb.toString()
    }
  }

  sealed trait Instr
  //Arithematic ops
  case class Add(rd: Register, rn: Register, op2: Operand) extends Instr {
    override def toString: String = "ADD " + rd + ", " + rn + ", " + op2
  }
  case class AddS(rd: Register, rn: Register, op2: Operand) extends Instr {
    override def toString: String = "ADDS " + rd + ", " + rn + ", " + op2
  }
  case class Sub(rd: Register, rn: Register, op2: Operand) extends Instr {
    override def toString: String = "SUB " + rd + ", " + rn + ", " + op2
  }
  case class SubS(rd: Register, rn: Register, op2: Operand) extends Instr {
    override def toString: String = "SUBS " + rd + ", " + rn + ", " + op2
  }
  case class SMul(rdLo: Register, rdHi: Register, rn: Register, rm: Register) extends Instr {
    override def toString: String =
    "SMULL " + rdLo + ", " + rdHi + ", " + rn + ", " + rm
  }
  case class RsbS(rd: Register, rn: Register, op2: Operand) extends Instr {
    override def toString: String = "RSBS " + rd + ", " + rn + ", " + op2
  }

  /* Comparison */
  case class Cmp(rn: Register, op2: Operand) extends Instr {
    override def toString: String = "CMP " + rn + ", " + op2
  }

  /* Logical Operations */
  case class And(rd: Register, rn: Register, op2: Operand) extends Instr {
    override def toString: String = "AND " + rd + ", " + rn + ", " + op2
  }
  case class Or(rd: Register, rn: Register, op2: Operand) extends Instr {
    override def toString: String = "ORR " + rd + ", " + rn + ", " + op2
  }
  case class Eor(rd: Register, rn: Register, op2: Operand) extends Instr {
    override def toString: String = "EOR " + rd + ", " + rn + ", " + op2
  }
  /* Branching */
  case class Branch(label: Label) extends Instr {
    override def toString: String = "B " + label
  }
  // Branch {Condition}
  case class BranchCond(cond: Condition, label: Label) extends Instr {
    override def toString: String = "B" + cond + " " + label
  }
  // Branch Link
  case class Bl(label: Label) extends Instr {
    override def toString: String = "BL " + label
  }
  // Branch Link {Condition}
  case class BranchLinkCond(cond: Condition, label: Label) extends Instr {
    override def toString: String = "BL" + cond + " " + label
  }

  /* Stack Operations */
  case class Push(rs: ListBuffer[Register]) extends Instr {
    override def toString: String = "PUSH " + "{" + rs.mkString(", ") + "}"
  }
  case class Pop(rs: ListBuffer[Register]) extends Instr {
    override def toString: String = "POP " + "{" + rs.mkString(", ") + "}"
  }
  //Move operations
  case class Mov(rd: Register, op2: Operand) extends Instr {
    override def toString: String = "MOV " + rd + ", " + op2
  }

  case class MovCond(cond: Condition, rd: Register, op2: Operand)
  extends Instr {
    override def toString: String = "MOV" + cond + " " + rd + ", " + op2
  }
  // Loading operations
  case class Ldr(rd: Register, op2: Operand) extends Instr {
    override def toString: String = "LDR " + rd + ", " + op2
  }
  object Ldr {
    def apply(isByte: Boolean, src: Register, dst: Register, offset: Int): Instr = {
      if (isByte) {
        return LdrSB.apply(src, dst, offset)
      }
      apply(src, dst, offset)
    }
    def apply(src: Register, dst: Register, offset: Int): Instr = {
      if (offset == 0) {
        return Ldr(src, RegAdd(dst))
      }
      Ldr(src, RegisterOffset(dst, offset))
    }
    def apply(isByte: Boolean, src: Register, op: Operand): Instr = {
      if (isByte) LdrSB(src, op) else Ldr(src, op)
    }
  }
  // Load Byte Signed
  case class LdrSB(rd: Register, op2: Operand) extends Instr {
    override def toString: String = "LDRSB " + rd + ", " + op2
  }
  object LdrSB {
    def apply(src: Register, dst: Register, offset: Int): Instr = {
      if (offset == 0) {
        return LdrSB(src, RegAdd(dst))
      }
      LdrSB(src, RegisterOffset(dst, offset))
    }
  }

  case class LdrCond(cond: Condition, rd: Register, op2: Operand)
    extends Instr {
    override def toString: String = "LDR" + cond + " " + rd + ", " + op2
  }
  //Storing operations
  case class Str(rd: Register, add: Address) extends Instr {
    override def toString: String = "STR " + rd + ", " + add
  }
  object Str {
    def apply(isByte: Boolean, src: Register, dst: Register, offset: Int): Instr = {
      if (isByte) {
        return StrB.apply(src, dst, offset)
      }
      apply(src, dst, offset)
    }
    def apply(src: Register, dst: Register, offset: Int): Instr = {
      if (offset == 0) {
        return Str(src, RegAdd(dst))
      }
      Str(src, RegisterOffset(dst, offset))
    }
    def apply(isByte: Boolean, src: Register, addr: Address): Instr = {
      if (isByte) StrB(src, addr) else Str(src, addr)
    }
  }
  //Storing byte
  case class StrB(rd: Register, add: Address) extends Instr {
    override def toString: String = "STRB " + rd + ", " + add
  }
  object StrB {
    def apply(src: Register, dst: Register, offset: Int): Instr = {
      if (offset == 0) {
        return StrB(src, RegAdd(dst))
      }
      StrB(src, RegisterOffset(dst, offset))
    }
  }

  case class StrOffsetIndex(rd: Register, regAdd: Register, offset: Int) extends Instr {
    override def toString: String =
    "STR " + rd + ", " + "[" + regAdd + ", #" + offset + "]!"
  }
  object StrOffsetIndex {
    def apply(isByte: Boolean, src: Register, dst: Register, offset: Int): Instr = {
      if (isByte) {
        return StrBOffsetIndex(src, dst, offset)
      }
      StrOffsetIndex(src, dst, offset)
    }
  }
  case class StrBOffsetIndex(rd: Register, regAdd: Register, offset: Int) extends Instr {
    override def toString: String =
    "STRB " + rd + ", " + "[" + regAdd + ", #" + offset + "]!"
  }

  case object Ltorg extends Instr {
    override def toString: String = ".ltorg"
  }
  //Labels
  case class Label(s: String) {
    override def toString: String = s
  }
  
  case class Data(label: Label, s: String)
}
