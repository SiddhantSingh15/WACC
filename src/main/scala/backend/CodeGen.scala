package backend

import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import backend.Operand._
import backend.CodeGeneration.ExpressionGen._
import backend.tableDataTypes._
import frontend.AST._
import frontend.SymbolTable
import backend.CodeGeneration.CodeGenHelper._
import backend.CodeGeneration.ArraysGen._
import backend.CodeGeneration.ReadGen._
import backend.CodeGeneration.FreeGen._
import backend.CodeGeneration.Functions._
import backend.CodeGeneration.PrintGen._
import backend.CodeGeneration.ScopeGen._
import backend.CodeGeneration.Assignments._

object CodeGen {

  // Flags for optimization techniques
  var constantEval = true

  // Values for code generation 
  var currSP = 0
  var scopeSP  = 0
  var currLabel: Label = _
  var symbTable: SymbolTable = _
  var dataTable = new DataTable
  var funcTable = new FunctionTable
  var preDefFuncTable = new FunctionTable
  var currInstructions = new BlockInstrs(ListBuffer.empty[Instr])

  val TRUE_INT = 1
  val FALSE_INT = 0

  val NO_OFFSET = 0
  val RESET_INT = 0

  val isPrintLn = true
  val isPrint = false
  
  //list of free registers
  private var freeRegisters: ListBuffer[Register] =
    ListBuffer(R4, R5, R6, R7, R8, R9, R10)

  final val resultRegister: Register = R0
  final val popRegister: Register = R11

  //translates statements into internal representation
  def transStat(stat: Stat): Unit = {
    stat match {
      case Read(assignLHS)                => transRead(assignLHS)
      case Free(expr)                     => transFree(expr)
      case Return(expr)                   => transReturn(expr)
      case Exit(expr)                     => transExit(expr)
      case Print(expr)                    => transPrint(expr, isPrint)
      case Println(expr)                  => transPrint(expr, isPrintLn)
      case If(expr, statThen, statElse)   => transIf(expr, statThen, statElse)
      case While(expr, stats)             => transWhile(expr, stats)
      case Begin(stats)                   => transBegin(stats)
      case AssignLR(assignLHS, assignRHS) => transAssignment(assignLHS, assignRHS)
      case TypeAssign(t, ident, rhs)      => translateDeclaration(t, ident, rhs)
      case _                              => 
    }
  }

  //translates Exit statement, first translate E into a free register, freeRegister, then Mov contents of freeRegister to resultRegister.
  private def transExit(expr: Expr): Unit = {
    val availReg = saveReg()
    transExp(expr, availReg)
    currInstructions.addAll(ListBuffer[Instr](Mov(resultRegister, availReg), Bl(Label("exit"))))
    restoreReg(availReg)
  }

  // Get a free register from freeRegisters list. If none available the popRegister is used.
  def saveReg(): Register = {
    if (freeRegisters.isEmpty) {
      return popRegister
    }
    val register = freeRegisters(0)
    freeRegisters.remove(0)
    register
  }

  ///* Add register back to freeRegisters list once finished with. */
  def restoreReg(reg: Register): Unit = {
    if (reg != popRegister) {
      reg +=: freeRegisters
    }
  }

  ///* Translates program into our internal representation. Output is used to generate .s file*/
  def transProgram(program: WaccProgram, symbTable: SymbolTable): (List[Data], List[(Label, BlockInstrs)]) = {

    this.symbTable = symbTable
    val WaccProgram(funcs, stats) = program

    for (i <- funcs.indices) {
      transFunction(funcs(i))
    }

    currLabel = Label("main")

    scopeSP = currSP
    val maxSpDepth = symbTable.spMaxDepth
    currSP += maxSpDepth
    currInstructions.add(Push(ListBuffer(R14_LR)))
    decrementSP(maxSpDepth)
    stats.foreach((s: Stat) => {
      transStat(s)
      }
    )
    
    incrementSP(currSP)
    currInstructions.addAll(ListBuffer(
      Ldr(resultRegister, Load_Mem(RESET_INT)),
      Pop(ListBuffer(R15_PC)),
      Ltorg))

    funcTable.add(currLabel, currInstructions)
    (dataTable.table.toList, (funcTable.table ++ preDefFuncTable.table).toList)
  }
}
