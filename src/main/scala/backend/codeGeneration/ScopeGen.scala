package backend.codeGeneration

import frontend.AST.{Stat, Expr}
import scala.collection.mutable.ListBuffer
import backend.Opcodes.{Instr, Cmp, Branch, BranchCond}
import backend.Condition.{EQ}
import backend.CodeGen._
import backend.Operand.{Imm_Int}
import backend.codeGeneration.ExpressionGen.transExp

object ScopeGen {

  /* 
   * Translates the BEGIN function and returns a list of instructions.
   */
	def transBegin(stats: List[Stat], currInstr: ListBuffer[Instr]): ListBuffer[Instr] = {
		transScope(stats, currInstr)
	}

  /*
   * Translates the IF function.
   * Takes expression, statThen, statElse and currInstr.
   * currInstr is the list of instructions that has been generated till the IF branch.
   * The function will append that list based on which branch to traverse.
   */
	def transIf(expr: Expr, statThen: List[Stat], statElse: List[Stat], currInstr: ListBuffer[Instr]): ListBuffer[Instr] = {
		val freeRegister = saveReg()
		currInstr ++= transExp(expr, freeRegister)
		currInstr += Cmp(freeRegister, Imm_Int(0))
		restoreReg(freeRegister)

		val elseLabel = funcTable.getNext()
		currInstr += BranchCond(EQ, elseLabel)

		val thenInstr = transScope(statThen, currInstr)
		val thenLabel = funcTable.getNext()

		thenInstr += Branch(thenLabel)
		funcTable.add(currLabel, thenInstr)		

		currLabel = elseLabel
		val elseInstrs = transScope(statElse, ListBuffer.empty[Instr])
		funcTable.add(currLabel, elseInstrs)

		currLabel = thenLabel
		ListBuffer.empty[Instr]
	}

  /*
   * Translates the WHILE function.
   * Takes expression, list of Stats and currInstr.
   * currInstr is the list of instructions that has been generated till the While branch.
   * The function will append that list based on which branch to traverse.
   */
	def transWhile(expr: Expr, stats: List[Stat], currInstr: ListBuffer[Instr]): ListBuffer[Instr] = {
		val instr = ListBuffer.empty[Instr]
		val nextLabel = funcTable.getNext()

		currInstr += Branch(nextLabel)
		funcTable.add(currLabel, currInstr)

		val bodyLabel = funcTable.getNext()
		currLabel = bodyLabel
		val transBody = transScope(stats, ListBuffer.empty[Instr])
		funcTable.add(currLabel, transBody)

		currLabel = nextLabel

		val register = saveReg()

		instr ++= transExp(expr, register)
		instr += Cmp(register, Imm_Int(1)) // TODO: remove magic number
		restoreReg(register)
		instr += BranchCond(EQ, bodyLabel)
	}

  /*
   * Translates only the stats in the current scope.
   */
  private def transScope(stats: List[Stat], currInstr: ListBuffer[Instr]): ListBuffer[Instr] = {
    symbTable = symbTable.getNextScope
    val oldScopeSp = SP_scope
    val scopeMaxSpDepth = symbTable.spMaxDepth
    currInstr ++= subSP(scopeMaxSpDepth)
    SP_scope = stackPointer
    stackPointer += scopeMaxSpDepth
    var instructions = currInstr
        stats.foreach((s: Stat) => {
          instructions = transStat(s, instructions)
        }
      )
    if (scopeMaxSpDepth > 0) {
      instructions ++= addSP(scopeMaxSpDepth)
      stackPointer -= scopeMaxSpDepth
    }
    SP_scope = oldScopeSp
    symbTable = symbTable.prev
    instructions
  }
}
