package backend.CodeGeneration

import frontend.AST._
import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import backend.Condition.{EQ}
import backend.CodeGen._
import backend.Operand.{Imm_Int}
import backend.CodeGeneration.ExpressionGen.transExp
import backend.CodeGeneration.CodeGenHelper._

object ScopeGen {

  /* 
   * Translates the BEGIN function and returns a list of instructions.
   */
	def transBegin(stats: List[Stat]): Unit = {
    val oldInBeginEndScope = inBeginEndScope
    inBeginEndScope = true
		transScope(stats)
    inBeginEndScope = oldInBeginEndScope
	}

  /*
   * Translates the IF function.
   * Takes expression, statThen, statElse and currInstr.
   * currInstr is the list of instructions that has been generated till the IF branch.
   * The function will append that list based on which branch to traverse.
   */
	def transIf(expr: Expr, statThen: List[Stat], statElse: List[Stat]): Unit = {
    if (controlFA) {
      val bool = reduceRHS(expr)
      bool match {
        case True =>
          transScope(statThen)
          return
        case False =>
          transScope(statElse)
          return
        case _ =>
      }
    }


		val freeRegister = saveReg()
		transExp(expr, freeRegister)
		currInstructions.add(Cmp(freeRegister, Imm_Int(0)))
		restoreReg(freeRegister)

		val elseLabel = funcTable.getNext()
		currInstructions.add(BranchCond(EQ, elseLabel))

    var oldInBeginEndScope = inBeginEndScope
    inBeginEndScope = false
		transScope(statThen)
		val postLabel = funcTable.getNext()

		currInstructions.add(Branch(postLabel))
		funcTable.add(currLabel, currInstructions)		

		currLabel = elseLabel
    currInstructions = BlockInstrs(ListBuffer.empty[Instr])

		transScope(statElse)
		funcTable.add(currLabel, currInstructions)

    inBeginEndScope = oldInBeginEndScope
		currLabel = postLabel
		currInstructions = BlockInstrs(ListBuffer.empty[Instr])
	}

  /*
   * Translates the WHILE function.
   * Takes expression, list of Stats and currInstr.
   * currInstr is the list of instructions that has been generated till the While branch.
   * The function will append that list based on which branch to traverse.
   */
	def transWhile(expr: Expr, stats: List[Stat]): Unit = {
    if (controlFA) {
      val bool = reduceRHS(expr)
      bool match {
        case False =>
          return
        case _ =>
      }
    }
		
		val nextLabel = funcTable.getNext()

		currInstructions.add(Branch(nextLabel))
		funcTable.add(currLabel, currInstructions)

		val bodyLabel = funcTable.getNext()
		currLabel = bodyLabel
    currInstructions = BlockInstrs(ListBuffer.empty[Instr])

    var oldInBeginEndScope = inBeginEndScope
    inBeginEndScope = false
		transScope(stats)
    inBeginEndScope = oldInBeginEndScope
    
		funcTable.add(currLabel, currInstructions)

		currLabel = nextLabel
    currInstructions = BlockInstrs(ListBuffer.empty[Instr])

		val register = saveReg()

		transExp(expr, register)
		currInstructions.add(Cmp(register, Imm_Int(TRUE_INT)))
		restoreReg(register)
		currInstructions.add(BranchCond(EQ, bodyLabel))
	}

  /*
   * Translates only the stats in the current scope.
   */
  def transScope(stats: List[Stat]): Unit = {
    symbTable = symbTable.getNextScope
    val oldScopeSp = scopeSP
    val scopeMaxSpDepth = symbTable.spMaxDepth
    decrementSP(scopeMaxSpDepth)
    scopeSP = currSP
    currSP += scopeMaxSpDepth
    stats.foreach((s: Stat) => {
          transStat(s)
        }
      )
    
    incrementSP(scopeMaxSpDepth)
    currSP -= scopeMaxSpDepth
    scopeSP = oldScopeSp
    symbTable = symbTable.prev
  }
}
