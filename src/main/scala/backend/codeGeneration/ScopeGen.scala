package backend.codeGeneration

import frontend.AST.{Stat, Expr}
import scala.collection.mutable.ListBuffer
import backend.Opcodes.{Instr, Cmp, Branch, BranchCond}
import backend.Condition.{EQ}
import backend.CodeGen._
import backend.Operand.{Imm_Int}
import backend.codeGeneration.ExpressionGen.transExp

object ScopeGen {
  private def transScope(stats: List[Stat]): ListBuffer[Instr] = {
    symbTable = symbTable.nextScope
	val instructions = ListBuffer.empty[Instr]
    stats.foreach((s: Stat) => {
      instructions ++= transStat(s)
    	}
    )
	symbTable = symbTable.prev
	instructions
  }

	def transBegin(stats: List[Stat]): ListBuffer[Instr] = {
		transScope(stats)
	}

	def transIf(expr: Expr, statThen: List[Stat], statElse: List[Stat]): ListBuffer[Instr] = {
		val freeRegister = saveReg()
		val instr = ListBuffer.empty[Instr]
		instr ++= transExp(expr, freeRegister)
		instr += Cmp(freeRegister, Imm_Int(0))
		restoreReg(freeRegister)

		val branch = funcTable.getNext()
		instr += BranchCond(EQ, branch)

		instr ++= transScope(statThen)
		val nextLabel = funcTable.getNext()

		instr += Branch(nextLabel)
		userTable.add(currLabel, instr)

		currLabel = branch

		val branchInstrs = transScope(statElse)
		userTable.add(currLabel, branchInstrs)

		currLabel = nextLabel
		ListBuffer.empty[Instr]
	}

	def transWhile(expr: Expr, stats: List[Stat]): ListBuffer[Instr] = {
		val instr = ListBuffer.empty[Instr]
		val nextLabel = funcTable.getNext()

		instr += Branch(nextLabel)
		userTable.add(currLabel, instr)

		val body = funcTable.getNext()
		currLabel = body

		val transBody = transScope(stats)
		userTable.add(currLabel, transBody)

		currLabel = nextLabel

		val register = saveReg()

		instr ++= transExp(expr, register)
		instr += Cmp(register, Imm_Int(1))
		restoreReg(register)
		instr += BranchCond(EQ, body)
	}
}
