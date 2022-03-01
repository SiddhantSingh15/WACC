package backend

import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import backend.Operand._
import backend.codeGeneration.ExpressionGen._
import backend.tableDataTypes._
import frontend.AST._
import frontend.SymbolTable

object CodeGen {

  // Values for code generation 
  var stackPointer = 0
  var currLabel = Label("main")
  var symbTable: SymbolTable = _
  var dataTable = new dataTable
  var funcTable = new functionTable
  var userTable = new functionTable
  
  val FALSE = 0
  val SIZE_ADDR = 4
  val SIZE_PAIR = SIZE_ADDR

  //rename
  var scopeSP  = 0
  var currSP = 0
  private val ERROR = -1

  final val generalRegisters: ListBuffer[Register] = 
    ListBuffer(R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12)

  private var freeRegisters: ListBuffer[Register] =
    ListBuffer(R4, R5, R6, R7, R8, R9, R10)

  final val resultRegister: Register = R0
  final val popRegister: Register = R11

  private var instrs: ListBuffer[Instr] = ListBuffer.empty[Instr]

  def transStat(stat: Stat, instructions: ListBuffer[Instr]): ListBuffer[Instr] = {
    stat match {
      case Read(assignLHS)                => // TODO
      case Free(expr)                     => // TODO
      case Return(expr)                   => // TODO
      case Exit(expr)                     => 
        instructions ++= transExit(expr)
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

  def incrementSP(toInc: Int): ListBuffer[Instr] = {
    val instrs = ListBuffer.empty[Instr]
    if (toInc == 0) {
      return instrs
    }
    var curToInc = toInc
    while (curToInc > MAX_INT_IMM) {
      curToInc -= MAX_INT_IMM
      instrs += backend.Opcodes.Add(R13_SP, R13_SP, Imm_Int(MAX_INT_IMM))
    }
    instrs += backend.Opcodes.Add(R13_SP, R13_SP, Imm_Int(curToInc))
    instrs
  }

  def decrementSP(toDec: Int): ListBuffer[Instr] = {
    val instrs = ListBuffer.empty[Instr]
    if (toDec == 0) {
      return instrs
    }
    var curToDec = toDec
    while (curToDec > MAX_INT_IMM) {
      curToDec -= MAX_INT_IMM
      instrs += backend.Opcodes.Sub(R13_SP, R13_SP, Imm_Int(MAX_INT_IMM))
    }
    instrs += backend.Opcodes.Sub(R13_SP, R13_SP, Imm_Int(curToDec))
    instrs
  }

  private def transExit(expr: Expr): ListBuffer[Instr] = {
    val availReg = saveReg()
    val instructions = ListBuffer.empty[Instr]
    instructions ++= transExp(expr, availReg)
    instructions ++= ListBuffer[Instr](Mov(resultRegister, availReg), Bl(Label("exit"))) 
    restoreReg(availReg)
    instructions
  }

  def saveReg(): Register = {
    if (freeRegisters.isEmpty) {
      return popRegister
    }
    val register = freeRegisters(0)
    freeRegisters.remove(0)
    register
  }

  def addFreeReg(reg: Register): Unit = {
    reg +=: freeRegisters
  }

  def restoreReg(reg: Register): Unit = {
    if (reg != popRegister) {
      reg +=: freeRegisters
    }
  }

  def transProgram(program: WaccProgram): (List[Data], List[(Label, List[Instr])]) = {

    this.symbTable = symbTable
    val WaccProgram(funcs, stats) = program

    // TODO: functions

    val instructions = ListBuffer[Instr](Push(ListBuffer(R14_LR)))
    stats.foreach((s: Stat) => {
      instructions ++= transStat(s, ListBuffer(Push(ListBuffer(R14_LR))))
    }
    )

    instructions ++= incrementSP(stackPointer)
    instructions ++= ListBuffer(
      Ldr(resultRegister, Load_Mem(0)), // TODO: magic number
      Pop(ListBuffer(R15_PC)),
      Ltorg
    )

    funcTable.add(currLabel, instructions)
    (dataTable.table.toList, funcTable.table.toList)
  }

  def getTypeSize(t: Type) : Int = {
    t match {
      case Int           => INT_SIZE
      case Bool          => BOOL_SIZE
      case CharType          => CHAR_SIZE
      case String        => STR_SIZE
      case ArrayType(innerT) => ARRAY_SIZE
      case Pair(_, _)     => PAIR_SIZE
      case _              => ERROR
  }
}

def isByte(t : Type): Boolean = {
  t == Bool || t == CharType
}

  val TRUE_INT = 1
  val FALSE_INT = 0

  val INT_SIZE = 4
  val CHAR_SIZE = 1
  val BOOL_SIZE = 1
  val STR_SIZE = 4
  val ADDRESS_SIZE = 4
  val ARRAY_SIZE = ADDRESS_SIZE
  val PAIR_SIZE = ADDRESS_SIZE
  val MAX_INT_IMM = 1024


}
