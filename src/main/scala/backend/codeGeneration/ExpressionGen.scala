package backend.codeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._
import backend.CodeGen._
import frontend.SymbolTable
import backend.Condition._
import backend.codeGeneration.ArraysGen._
import backend.DefinedFuncs.RuntimeErrors._
import backend.DefinedFuncs.PreDefinedFuncs._

import scala.collection.mutable.ListBuffer

object ExpressionGen {

    val INT_TRUE = 1
    val INT_FALSE = 0

    def boolToInt(bool: BoolLiter): Int = {
        bool match {
            case True => INT_TRUE
            case False => INT_FALSE
        }
    }

    def transExp(expr: Expr, rd: Register): ListBuffer[Instr] = {
        val instructions = ListBuffer.empty[Instr]    
        expr match {
            case IntLiter(number) =>
                val (reg, instrs) = collectRegister(rd)
                instructions ++= instrs
                instructions += Ldr(reg, Load_Mem(number))
            case bool: BoolLiter =>
                instructions += Mov(rd, Imm_Int(boolToInt(bool)))

            case CharLiter(character)=>
                ListBuffer(Mov(rd, Imm_Char(character.toString().charAt(0))))

            case str: StrLiter => 
                val label = dataTable.addStrLiter(str)
                ListBuffer(Ldr(rd, DataLabel(label)))

            case PairLiter() => 
                ListBuffer(Ldr(rd, Load_Mem(0))) // TODO: remove magic number

            case ident: Ident => 
                val (i, t) = symbTable(ident)
                val offset = SP_curr - i
                instructions += Ldr(isByte(t), rd, R13_SP, offset)

            case ArrayElem(id, exprs) => loadArrayElem(id, exprs, rd)
            case unOp: UnOp =>
                transUnOp(unOp, rd)

            case binOp: BinOp =>
                transBinOp(binOp, rd)

            case _ => ListBuffer.empty[Instr]
        }
    }

    def transUnOp(op: UnOp, rd: Register): ListBuffer[Instr] = {
        op match {
            case Not(expr) =>
                transExp(expr, rd) += Eor(rd, rd, Imm_Int(INT_TRUE))
            case Negation(expr) =>
                transExp(expr, rd) ++= ListBuffer(
                    RsbS(rd, rd, Imm_Int(0)),
                    BranchLinkCond(OF, addRTE(Overflow))
                )
            case Len(ident: Ident) =>
                val (i, t) = symbTable(ident)
                ListBuffer(
                    Ldr(rd, RegisterOffset(R13_SP, stackPointer - i)), // TODO: stack pointer
                    Ldr(rd, RegAdd(rd))
                )
            case Ord(expr) =>
                transExp(expr, rd)
            case Chr(expr) =>
                transExp(expr, rd)
            case _  =>
                ListBuffer.empty[Instr]
        }
    }
    
    private def collectRegister(rd: Register): (Register, ListBuffer[Instr]) = {
      val instructions = ListBuffer.empty[Instr]

      var reg = rd

      if (reg == popRegister) {
        instructions += Push(ListBuffer(R10))
        reg = R10
      }

      (reg, instructions)
    }

    def transBinOp(op: BinOp, rn: Register): ListBuffer[Instr] = {
        val instructions = ListBuffer.empty[Instr]

        instructions ++= transExp(op.exp1, rn)
        val rm = saveReg()
        instructions ++= transExp(op.exp2, rm)

        // Check over allocation of register
        var rd = rn
        if (popRegister == rm) {
            instructions += Pop(ListBuffer(popRegister))
            rd = R10
        }

        op match {
            case mathOp: MathFuncs => 
                instructions ++= transMathOp(mathOp, rd, rm)
            case cmpOp: CompareFuncs => 
                instructions ++= transCmpEqOp(cmpOp, rd, rm)
            case eqOp: EqualityFuncs => 
                instructions ++= transCmpEqOp(eqOp, rd, rm)
            case lgOp: LogicFuncs => 
                instructions += transLgOp(lgOp, rd, rm)
        }
    }

    def transMathOp(op: MathFuncs, rd: Register, rm: Register): ListBuffer[Instr] = {

        op match {
            case frontend.AST.Mul(_,_) =>
                ListBuffer(
                    SMul(rd, rm, rd, rm),
                    Cmp(rm, ASR(rd, Imm_Int(31))), // TODO: Remove magic number
                    BranchLinkCond(NE, addRTE(Overflow))
                )
            case frontend.AST.Div(_,_) =>
                ListBuffer(
                    Mov(resultRegister, rd),
                    Mov(R1, rm), // need to be in R0 and R1 for __aeabi_idiv
                    Bl(addRTE(DivideByZero)),
                    Bl(Label("__aeabi_idiv")),
                    Mov(rd, resultRegister)
                )
            case frontend.AST.Mod(_,_) => 
                ListBuffer(
                    Mov(resultRegister, rd),
                    Mov(R1, rm),
                    Bl(addRTE(DivideByZero)),
                    Bl(Label("__aeabi_idivmod")),
                    Mov(rd, R1)
                )
            case frontend.AST.Plus(_,_) =>
                ListBuffer(
                    AddS(rd, rd, rm),
                    BranchLinkCond(OF, addRTE(Overflow))
                )
            case frontend.AST.Sub(_,_) =>
                ListBuffer(
                    SubS(rd, rd, rm),
                    BranchLinkCond(OF, addRTE(Overflow))
                )

        }
    }

    def transCmpEqOp(op: BinOp, rd: Register, rm: Register): ListBuffer[Instr] = {
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

        ListBuffer(
            Cmp(rd, rm),
            MovCond(cond, rd, Imm_Int(INT_FALSE)),
            MovCond(cond.oppositeCmp, rd, Imm_Int(INT_FALSE))
        )
    }

    def transLgOp(op: LogicFuncs, rd: Register, rm: Register): Instr = {
        op match {
            case frontend.AST.And(_,_) => backend.Opcodes.And(rd, rd, rm)
            case frontend.AST.Or(_,_) => backend.Opcodes.Or(rd, rd, rm)
        }
    }
}
