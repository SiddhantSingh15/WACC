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
  
  val SIZE_INT = 4
  val SIZE_CHAR = 1
  val SIZE_BOOL = 1
  val SIZE_STR = 4

  val FALSE = 0
  val SIZE_ADDR = 4
  val SIZE_PAIR = SIZE_ADDR
  val SIZE_ARR = SIZE_ADDR

  val MAX_IMM_INT = 1024
  
  var SP_scope  = 0
  var SP_curr = 0
  val NO_OFFSET = 0
  
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
    while (curToInc > MAX_IMM_INT) {
      curToInc -= MAX_IMM_INT
      instrs += backend.Opcodes.Add(R13_SP, R13_SP, Imm_Int(MAX_IMM_INT))
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
    while (curToDec > MAX_IMM_INT) {
      curToDec -= MAX_IMM_INT
      instrs += backend.Opcodes.Sub(R13_SP, R13_SP, Imm_Int(MAX_IMM_INT))
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

  def subSP(sp: Int): ListBuffer[Instr] = {
    val instructions = ListBuffer.empty[Instr]

    if (sp == 0) {
      return instructions
    }

    var currSP = sp

    while (currSP > MAX_IMM_INT) {
      currSP -= MAX_IMM_INT
      instructions += Opcodes.Sub(R13_SP, R13_SP, Imm_Int(MAX_IMM_INT))
    }

    instructions += Opcodes.Sub(R13_SP, R13_SP, Imm_Int(currSP))
    instructions
  }

  def addSP(sp: Int): ListBuffer[Instr] = {
    val instructions = ListBuffer.empty[Instr]

    if (sp == 0) {
      return instructions
    }

    var currSP = sp

    while (currSP > MAX_IMM_INT) {
      currSP -= MAX_IMM_INT
      instructions += Opcodes.Add(R13_SP, R13_SP, Imm_Int(MAX_IMM_INT))
    }

    instructions += Opcodes.Add(R13_SP, R13_SP, Imm_Int(currSP))
    instructions
  }

  def typeConverter(expr: Expr): Type = {
    expr match {
      case _: EqualityFuncs => Bool
      
      case _: CompareFuncs => Bool
      case _: LogicFuncs   => Bool
      case _: Not          => Bool
      case _: BoolLiter    => Bool


      case _: Negation     => Int
      case _: MathFuncs    => Int
      case _: Len          => Int
      case _: Ord          => Int
      case _: IntLiter     => Int
      
      case _: PairLiter    => Pair(null, null)

      case _: StrLiter     => String

      case _: Chr          => CharType
      case _: CharLiter    => CharType


      case ident: Ident    =>
        val (_, tpe) = symbTable(ident)
        tpe
      
      case ArrayElem(ident, exprList) => 
        var (_, tpe) = symbTable(ident)
        tpe = exprList.foldLeft(tpe)((t, _) => typeOf(t))
        tpe
    }
  }

  private def typeOf(tpe: Type): Type = tpe match {
    case ArrayType(t) => t
  }

  def getTypeSize(t: Type) : Int = {
    t match {
      case Int               => SIZE_INT
      case Bool              => SIZE_BOOL
      case CharType          => SIZE_CHAR
      case String            => SIZE_STR
      case ArrayType(innerT) => SIZE_ARR
      case Pair(_, _)        => SIZE_PAIR
      case _                 => ERROR
    }
  }

  def isByte(t : Type): Boolean = {
    t == Bool || t == CharType
  }
}
