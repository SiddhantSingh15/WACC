package backend

import scala.collection.mutable.ListBuffer
import backend.Opcodes._
import backend.Operand._
import backend.CodeGeneration.ExpressionGen._
import backend.tableDataTypes._
import frontend.AST._
import frontend.SymbolTable
import backend.CodeGeneration.ArraysGen._
import backend.CodeGeneration.ReadGen._
import backend.CodeGeneration.FreeGen._
import backend.CodeGeneration.Functions._
import backend.CodeGeneration.PrintGen._
import backend.CodeGeneration.ScopeGen._
import backend.CodeGeneration.Assignments._

object CodeGen {

  // Values for code generation 
  var stackPointer = 0
  var currLabel: Label = _
  var symbTable: SymbolTable = _
  var dataTable = new dataTable
  var funcTable = new FunctionTable
  var preDefFuncTable = new FunctionTable
  var currInstructions = ListBuffer.empty[Instr]
  
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
  val NO_OFFSET = 0
  
  private val ERROR = -1
  
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
      case Print(expr)                    => transPrint(expr, false) // TODO: remove magic boolean
      case Println(expr)                  => transPrint(expr, true)
      case If(expr, statThen, statElse)   => transIf(expr, statThen, statElse)
      case While(expr, stats)             => transWhile(expr, stats)
      case Begin(stats)                   => transBegin(stats)
      case AssignLR(assignLHS, assignRHS) => transAssignment(assignLHS, assignRHS)
      case TypeAssign(t, ident, rhs)      => translateDeclaration(t, ident, rhs)
      case _                              => 
    }
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
  //translates Exit statement, first translate E into a free register, freeRegister, then Mov contents of freeRegister to resultRegister.
  private def transExit(expr: Expr): Unit = {
    val availReg = saveReg()
    transExp(expr, availReg)
    currInstructions ++= ListBuffer[Instr](Mov(resultRegister, availReg), Bl(Label("exit"))) 
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
  def transProgram(program: WaccProgram, symbTable: SymbolTable): (List[Data], List[(Label, List[Instr])]) = {

    this.symbTable = symbTable
    val WaccProgram(funcs, stats) = program

    for (i <- funcs.indices) {
      transFunction(funcs(i))
    }

    currLabel = Label("main")

    SP_scope = stackPointer
    val maxSpDepth = symbTable.spMaxDepth
    stackPointer += maxSpDepth
    currInstructions += Push(ListBuffer(R14_LR))
    currInstructions ++= decrementSP(maxSpDepth)
    stats.foreach((s: Stat) => {
      transStat(s)
      }
    )
    
    currInstructions ++= incrementSP(stackPointer)
    currInstructions ++= ListBuffer(
      Ldr(resultRegister, Load_Mem(0)), // TODO: magic number
      Pop(ListBuffer(R15_PC)),
      Ltorg
    )

    funcTable.add(currLabel, currInstructions)
    (dataTable.table.toList, (funcTable.table ++ preDefFuncTable.table).toList)
  }

  private def typeOf(tpe: Type): Type = tpe match {
    case ArrayType(t) => t
    case _ => tpe
  }
  // returns size for a particular type
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
  // gets type for a particular expr
  def getExprType(expr: Expr): Type = {
    expr match {
      case _: IntLiter         => Int
      case _: BoolLiter        => Bool
      case _: CharLiter        => CharType
      case StrLiter(_)         => String
      case PairLiter()         => Pair(null, null)
      case id: Ident           => 
        val (_, t) = symbTable(id)
        t
      case ArrayElem(id, exprs) =>  
        var (_, t) = symbTable(id)
        t = exprs.foldLeft(t)((x, _) => getInnerType(x))
        t

      // Unary Operators
      case Not(_)      => Bool
      case Negation(_) => Int
      case Len(_)      => Int
      case Ord(_)      => Int
      case Chr(_)      => CharType

      // Binary Operators
      case _: MathFuncs     => Int
      case _: EqualityFuncs => Bool
      case _: LogicFuncs    => Bool
      case _: CompareFuncs  => Bool
      case _                => ???
    }
  }
}
