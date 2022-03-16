package backend.CodeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._
import backend.CodeGen._
import frontend.SymbolTable
import backend.Condition._
import backend.CodeGeneration.ArraysGen._
import backend.CodeGeneration.CodeGenHelper._
import backend.DefinedFuncs.RuntimeErrors._
import backend.DefinedFuncs.PreDefinedFuncs._

import scala.collection.mutable.ListBuffer

object ExpressionGen {

  val MUL_INT = 31
  val NULL_INT = 0
  
 /* Mapping True -> 1, False -> 0 */
  def boolToInt(bool: BoolLiter): Int = {
    bool match {
      case True => TRUE_INT
      case False => FALSE_INT
    }
  }

  /* Translating an Expr to the ARM language */
  def transExp(expr: Expr, rd: Register): Unit = {
    expr match {
      case IntLiter(number) =>
        val reg = collectRegister(rd)
        currInstructions.add(Ldr(reg, Load_Mem(number)))
      case bool: BoolLiter =>
        currInstructions.add(Mov(rd, Imm_Int(boolToInt(bool))))

      case character: CharLiter =>
        currInstructions.add(Mov(rd, Imm_Char(character.toString.charAt(1))))

      case str: StrLiter => 
        val label = dataTable.addStrLiter(str)
        currInstructions.add(Ldr(rd, DataLabel(label)))

      case PairLiter() => 
        currInstructions.add(Ldr(rd, Load_Mem(NULL_INT))) 

      case ident: Ident => 
        val (i, t) = symbTable(ident)
        val offset = currSP - i
        currInstructions.add(Ldr(isByte(t), rd, R13_SP, offset))

      case ArrayElem(id, exprs) => 
        loadArrayElem(id, exprs, rd)
      case unOp: UnOp =>
        transUnOp(unOp, rd)

      case binOp: BinOp =>
        transBinOp(binOp, rd)

      case DerefPointer(ptr) =>
        transExp(ptr, rd)
        currInstructions.add(Ldr(rd, RegAdd(rd)))

      case _ =>
    }
  }

  /* Translating a unary operator to the ARM language */
  def transUnOp(op: UnOp, rd: Register): Unit = {
    op match {
      case Not(expr) =>
        transExp(expr, rd)
        currInstructions.add(Eor(rd, rd, Imm_Int(TRUE_INT)))
      case Negation(expr) =>
        transExp(expr, rd) 
        currInstructions.addAll(ListBuffer[Instr](
          RsbS(rd, rd, Imm_Int(0)),
          BranchLinkCond(OF, addRTE(Overflow))
        ))
      case Len(ident: Ident) =>
        val (i, t) = symbTable(ident)
        currInstructions.addAll(ListBuffer[Instr](
          Ldr(rd, RegisterOffset(R13_SP, currSP - i)),
          Ldr(rd, RegAdd(rd))
        ))
  
      case Ord(expr) =>
        transExp(expr, rd)
      case Chr(expr) =>
        transExp(expr, rd)
      case _  =>
    }
  }
    
  /* Check if registers are full, if so pushes contents of R10 to stack */
  private def collectRegister(rd: Register): Register = {

    var reg = rd

    if (rd == popRegister) {
      currInstructions.add(Push(ListBuffer(R10)))
      reg = R10
    }

    reg
  }

  private def transMemLoc(ptr: Expr, register: Register): Unit = {
    ptr match {
      case ident: Ident        =>
        val (i, tpe) = symbTable(ident)
        val offset = currSP - i
        currInstructions.add(Add(register, R13_SP, Imm_Int(offset)))
      case DerefPointer(inTpe) =>
        transExp(inTpe, register)
      case _                   => ???
    }
  }

  def transBinOp(op: BinOp, rn: Register): Unit = {

    transExp(op.exp1, rn)
    val rm = saveReg()
    transExp(op.exp2, rm)

    // Check over allocation of register
    var rd = rn
    if (popRegister == rm) {
      currInstructions.add(Pop(ListBuffer(popRegister)))
      rd = R10
    }

    op match {
      case mathOp: MathFuncs => 
        transMathOp(mathOp, rd, rm)
      case cmpOp: CompareFuncs => 
        transCmpEqOp(cmpOp, rd, rm)
      case eqOp: EqualityFuncs => 
        transCmpEqOp(eqOp, rd, rm)
      case lgOp: LogicFuncs => 
        transLgOp(lgOp, rd, rm)
    }
    restoreReg(rm)
  }

  /* Translating a math op to the ARM language */
  def transMathOp(op: MathFuncs, rd: Register, rm: Register): Unit = {

    op match {
      case frontend.AST.Mul(_,_) =>
        currInstructions.addAll(ListBuffer(
          SMul(rd, rm, rd, rm),
          Cmp(rm, ASR(rd, Imm_Int(MUL_INT))),
          BranchLinkCond(NE, addRTE(Overflow))
        ))
      case frontend.AST.Div(_,_) =>
        currInstructions.addAll(ListBuffer(
          Mov(resultRegister, rd),
          Mov(R1, rm), // need to be in R0 and R1 for __aeabi_idiv
          Bl(addRTE(DivideByZero)),
          Bl(Label("__aeabi_idiv")),
          Mov(rd, resultRegister)
        ))
      case frontend.AST.Mod(_,_) => 
        currInstructions.addAll(ListBuffer(
          Mov(resultRegister, rd),
          Mov(R1, rm),
          Bl(addRTE(DivideByZero)),
          Bl(Label("__aeabi_idivmod")),
          Mov(rd, R1)
        ))
      case frontend.AST.Plus(_,_) =>
        currInstructions.addAll(ListBuffer(
          AddS(rd, rd, rm),
          BranchLinkCond(OF, addRTE(Overflow))
        ))
      case frontend.AST.Sub(_,_) =>
        currInstructions.addAll(ListBuffer(
          SubS(rd, rd, rm),
          BranchLinkCond(OF, addRTE(Overflow))
        ))
    }
  }

  /* Translating a comparison operator to the ARM language */
  def transCmpEqOp(op: BinOp, rd: Register, rm: Register): Unit = {
    var cond: Condition = null;
    op match {
      case frontend.AST.GT(_,_) => cond = backend.Condition.GT
      case frontend.AST.GTE(_,_) => cond = backend.Condition.GE
      case frontend.AST.LT(_,_) => cond = backend.Condition.LT
      case frontend.AST.LTE(_,_) => cond = backend.Condition.LE
      case frontend.AST.Equal(_,_) => cond = backend.Condition.EQ
      case frontend.AST.NotEqual(_,_) => cond = backend.Condition.NE
      case _ =>
    }

    currInstructions.addAll(ListBuffer(
      Cmp(rd, rm),
      MovCond(cond, rd, Imm_Int(TRUE_INT)),
      MovCond(cond.oppositeCmp, rd, Imm_Int(FALSE_INT))
    ))
  }

  /* Translating a logic operator to the ARM language  */
  def transLgOp(op: LogicFuncs, rd: Register, rm: Register): Unit = {
    op match {
      case frontend.AST.And(_,_) => currInstructions.add(backend.Opcodes.And(rd, rd, rm))
      case frontend.AST.Or(_,_) => currInstructions.add(backend.Opcodes.Or(rd, rd, rm))
    }
  }
}