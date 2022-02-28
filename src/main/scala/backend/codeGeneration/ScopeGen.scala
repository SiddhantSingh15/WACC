package backend.codeGeneration

import frontend.AST.{Stat, Expr}
import scala.collection.mutable.ListBuffer
import backend.Opcodes.{Instr, Cmp, Branch, BranchCond}
import backend.Condition.{EQ}
import backend.CodeGen._
import backend.Operand.{Imm_Int}
import backend.codeGeneration.ExpressionGen.transExp

object ScopeGen {
  private def transScope(stat: Stat, instrs: ListBuffer[Instr]): ListBuffer[Instr] = {
    symbTable = symbTable.nextScope
		val instructions = transStat(stat, instrs)
		symbTable = symbTable.prev
		instructions
  }

	def transBegin(stat: Stat, instr: ListBuffer[Instr]): ListBuffer[Instr] = {
		transScope(stat, instr)
	}

	def transIf(expr: Expr, statThen: Stat, statElse: Stat, instr: ListBuffer[Instr]): ListBuffer[Instr] = {
		val freeRegister = saveReg()
		instr ++= transExp(expr, freeRegister)
		instr += Cmp(freeRegister, Imm_Int(0))
		addFreeReg(freeRegister)

		val branch = funcTable.getNext()
		instr += BranchCond(EQ, branch)

		val instructions = transScope(statThen, instr)
		val nextLabel = funcTable.getNext()

		instructions += Branch(nextLabel)
		userTable.add(currLabel, instructions)

		currLabel = branch

		val branchInstrs = transScope(statElse, ListBuffer.empty[Instr])
		userTable.add(currLabel, branchInstrs)

		currLabel = nextLabel
		ListBuffer.empty[Instr]
	}

	def transWhile(expr: Expr, stat: Stat, instr: ListBuffer[Instr]): ListBuffer[Instr] = {
		val instructions = ListBuffer.empty[Instr]
		val nextLabel = funcTable.getNext()

		instr += Branch(nextLabel)
		userTable.add(currLabel, instr)

		val body = funcTable.getNext()
		currLabel = body

		val transBody = transScope(stat, ListBuffer.empty[Instr])
		userTable.add(currLabel, transBody)

		currLabel = nextLabel

		val register = saveReg()

		instructions ++= transExp(expr, register)
		instructions += Cmp(register, Imm_Int(1))
		addFreeReg(register)
		instructions += BranchCond(EQ, body)
	}
}
