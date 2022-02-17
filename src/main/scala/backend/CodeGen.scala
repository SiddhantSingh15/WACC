package backend

import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import frontend.AST._

object CodeGen {
  final val generalRegisters: ListBuffer[Register] = 
    ListBuffer(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12)

  final val resultRegister: Register = R0

  private var instrs: ListBuffer[Instr] = ListBuffer.empty[Instr]

  private def transExp(exp: Expr): ListBuffer[Instr] = {
    ListBuffer.empty[Instr]
  }

  private def transStat(stat: Stat, 
    regList: ListBuffer[Register]): ListBuffer[Instr] = {
    stat match {
      case Read(assignLHS)                => // TODO
      case Free(expr)                     => // TODO
      case Return(expr)                   => // TODO
      case Exit(expr)                     => 
        return transExit(expr, regList)
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

  private def transExit(expr: Expr, 
    regList: ListBuffer[Register]): ListBuffer[Instr] = {
      expr match {
        case IntLiter(number) => 
          val availReg = regList.filter(reg => reg != R0)(0)
          ListBuffer[Instr](Ldr(availReg, Load_Mem(number)), Mov(R0, availReg), Bl("exit"))
        case _                => transExp(expr)
      }
  }

  private def saveRegisters(regsNotInUse: ListBuffer[Register]): Instr = {
    Push(generalRegisters.filter(reg => !regsNotInUse.contains(reg)))
  }

  private def restoreRegisters(regsNotInUse: ListBuffer[Register]): Instr = {
    Pop(generalRegisters.filter(reg => !regsNotInUse.contains(reg)).reverse)
  }


}
