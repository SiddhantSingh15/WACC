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
	def transBegin(stats: List[Stat]): Unit = {
		transScope(stats)
	}

  /*
   * Translates the IF function.
   * Takes expression, statThen, statElse and currInstr.
   * currInstr is the list of instructions that has been generated till the IF branch.
   * The function will append that list based on which branch to traverse.
   */
	def transIf(expr: Expr, statThen: List[Stat], statElse: List[Stat]): Unit = {
		val freeRegister = saveReg()
		transExp(expr, freeRegister)
		currInstructions += Cmp(freeRegister, Imm_Int(0))
		restoreReg(freeRegister)

		val elseLabel = funcTable.getNext()
		currInstructions += BranchCond(EQ, elseLabel)

		transScope(statThen)
		val thenLabel = funcTable.getNext()

		currInstructions += Branch(thenLabel)
		funcTable.add(currLabel, currInstructions)		

		currLabel = elseLabel
    currInstructions = ListBuffer.empty[Instr]
		transScope(statElse)
		funcTable.add(currLabel, currInstructions)

		currLabel = thenLabel
		currInstructions = ListBuffer.empty[Instr]
	}

  /*
   * Translates the WHILE function.
   * Takes expression, list of Stats and currInstr.
   * currInstr is the list of instructions that has been generated till the While branch.
   * The function will append that list based on which branch to traverse.
   */
	def transWhile(expr: Expr, stats: List[Stat]): Unit = {
		
		val nextLabel = funcTable.getNext()

		currInstructions += Branch(nextLabel)
		funcTable.add(currLabel, currInstructions)

		val bodyLabel = funcTable.getNext()
		currLabel = bodyLabel
    currInstructions = ListBuffer.empty[Instr]
		transScope(stats)
		funcTable.add(currLabel, currInstructions)

		currLabel = nextLabel
    currInstructions = ListBuffer.empty[Instr]

		val register = saveReg()

		transExp(expr, register)
		currInstructions += Cmp(register, Imm_Int(1)) // TODO: remove magic number
		restoreReg(register)
		currInstructions += BranchCond(EQ, bodyLabel)
	}

  /*
   * Translates only the stats in the current scope.
   */
  private def transScope(stats: List[Stat]): Unit = {
    symbTable = symbTable.getNextScope
    val oldScopeSp = SP_scope
    val scopeMaxSpDepth = symbTable.spMaxDepth
    currInstructions ++= decrementSP(scopeMaxSpDepth)
    SP_scope = stackPointer
    stackPointer += scopeMaxSpDepth
    stats.foreach((s: Stat) => {
          transStat(s)
        }
      )
    if (scopeMaxSpDepth > 0) {
      currInstructions ++= incrementSP(scopeMaxSpDepth)
      stackPointer -= scopeMaxSpDepth
    }
    SP_scope = oldScopeSp
    symbTable = symbTable.prev
  }
}
