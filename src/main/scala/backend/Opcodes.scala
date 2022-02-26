package backend

import backend.Operand._
import scala.collection.mutable.ListBuffer

object Opcodes {
	sealed trait Instr 

	// ARITHMETIC

	case class Add(rd: Register, rn: Register, op2: Operand) extends Instr
	case class Sub(rd: Register, rn: Register, op2: Operand) extends Instr
	case class Mul(rd: Register, rm: Register, rs: Register) extends Instr

	// COMPARE

	case class Cmp(rn: Register, op2: Operand) extends Instr

	// BRANCH

	case class B(label: String) extends Instr
	case class Bl(label: String) extends Instr

	// LOGICAL

	case class Mov(rd: Register, op2: Operand) extends Instr
	case class And(rd: Register, rn: Register, op2: Operand) extends Instr
	case class Xor(rd: Register, rn: Register, op2: Operand) extends Instr
	case class Or(rd: Register, rn: Register, op2: Operand) extends Instr

	// PUSH/ POP
	case class Push(regList: ListBuffer[Register]) extends Instr {
		override def toString: String = s"Push {${regList.mkString(", ")}}"
	}
	case class Pop(regList: ListBuffer[Register]) extends Instr {
		override def toString: String = s"POP {${regList.mkString(", ")}}"
	}

	// LOADING
	case class Ldr(rd: Register, op2: Operand) extends Instr {
		override def toString: String = s"LDR $rd, $op2"
	}

	
	case object Ltorg extends Instr {
		override def toString: String = ".ltorg"
	}

	// Labels
	case class Label(s: String) {
		override def toString: String = s
	}
	case class Data(label: Label, s: String)
}
