package backend

import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import backend.Operand._
import backend.codeGeneration.ExpressionGen._
import backend.tableDataTypes._
import frontend.AST._

object CodeGen {

  // Values for code generation 
  var stackPointer = 0
  var currLabel = Label("main")
  var funcTable = new functionTable

  final val generalRegisters: ListBuffer[Register] = 
    ListBuffer(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12)

  private var freeRegisters: ListBuffer[Register] =
    ListBuffer(R4, R5, R6, R7, R8, R9, R10)

  final val resultRegister: Register = R0

  private var instrs: ListBuffer[Instr] = ListBuffer.empty[Instr]

  private def transStat(stat: Stat): ListBuffer[Instr] = {
    stat match {
      case Read(assignLHS)                => // TODO
      case Free(expr)                     => // TODO
      case Return(expr)                   => // TODO
      case Exit(expr)                     => 
        return transExit(expr)
      case Print(expr)                    => // TODO
      case Println(expr)                  => // TODO
      case If(expr, statThen, statElse)   => // TODO
      case While(expr, stats)             => // TODO
      case Begin(stats)                   => // TODO
      case AssignLR(assignLHS, assignRHS) => // TODO
      case TypeAssign(t, ident, rhs)      => // TODO
      case _                              => 
    }
    ListBuffer.empty[Instr]
  }

  private def transExit(expr: Expr): ListBuffer[Instr] = {
    val availReg = freeRegisters(0)
    val instructions = ListBuffer.empty[Instr]
    freeRegisters.remove(0)
    instructions ++= transExp(expr, availReg)
    instructions ++= ListBuffer[Instr](Mov(resultRegister, availReg), Bl(Label("exit"))) 
    availReg +=: freeRegisters
    instructions
  }

  private def saveRegisters(regsNotInUse: ListBuffer[Register]): Instr = {
    Push(generalRegisters.filter(reg => !regsNotInUse.contains(reg)))
  }

  private def restoreRegisters(regsNotInUse: ListBuffer[Register]): Instr = {
    Pop(generalRegisters.filter(reg => !regsNotInUse.contains(reg)).reverse)
  }

  def transProgram(program: WaccProgram): List[(Label, List[Instr])] = {

    val WaccProgram(funcs, stats) = program

    // TODO: functions

    val instructions = ListBuffer[Instr](Push(ListBuffer(R14_LR)))

    stats.foreach((s: Stat) => {
      instructions ++= transStat(s)
    }
    )

    instructions ++= ListBuffer(
      Ldr(resultRegister, Load_Mem(0)), // TODO: magic number
      Pop(ListBuffer(R15_PC)),
      Ltorg
    )

    funcTable.add(currLabel, instructions)
    funcTable.table.toList
  }


}
