package backend.codeGeneration

import backend.Operand._
import frontend.AST._
import backend.Opcodes._
import backend.CodeGen._
import frontend.SymbolTable
import backend.Condition._

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
    
        expr match {
            case IntLiter(number) =>
                ListBuffer(Ldr(rd, Load_Mem(number)))

            case bool: BoolLiter =>
                ListBuffer(Mov(rd, Imm_Int(boolToInt(bool))))

            case CharLiter(character) =>
                ListBuffer(Mov(rd, Imm_Char(character)))

            case StrLiter(string) => 
                val label = dataTable.addData(string)
                ListBuffer(Ldr(rd, DataLabel(label)))

            case PairLiter() => 
                ListBuffer(Ldr(rd, Load_Mem(0))) // TODO: remove magic number

            case Ident(id) =>
            case ArrayElem(id, exprs) =>
            case unOp: UnOp =>
                transUnOp(unOp, rd)

            case binOp: BinOp =>
                transBinOp(binOp, rd)

            case _ => ListBuffer.empty[Instr]
        }
        ListBuffer.empty[Instr]
    }

    def transUnOp(op: UnOp, rd: Register): ListBuffer[Instr] = {
        op match {
            case Not(expr) =>
                transExp(expr, rd) += Eor(rd, rd, Imm_Int(INT_TRUE))
            case Negation(expr) =>
                transExp(expr, rd) ++= ListBuffer(
                    RsbS(rd, rd, Imm_Int(0)) // remove magic number
                 // , BranchLinkCond(OF, RunTimeRror) TODO: runtime errors
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
            case Mul =>
                ListBuffer(
                    SMul(rd, rm, rd, rm),
                    Cmp(rm, ASR(rd, Imm_Int(31))) // TODO: Remove magic number
                    // TODO: stuff for runtime error
                )
            case Div =>
                ListBuffer(
                    Mov(resultRegister, rd),
                    Mov(R1, rm), // need to be in R0 and R1 for __aeabi_idiv
                    // TODO: stuff for runtime error
                    Bl(Label("__aeabi_idiv")),
                    Mov(rd, resultRegister)
                )
            case Mod => 
                ListBuffer(
                    Mov(resultRegister, rd),
                    Mov(R1, rm),
                    // TODO: stuff for runtime error
                    Bl(Label("__aeabi_idivmod")),
                    Mov(rd, R1)
                )
            case Plus =>
                ListBuffer(
                    AddS(rd, rd, rm)
                    // TODO: stuff for runtime error
                )
            case Sub =>
                ListBuffer(
                    SubS(rd, rd, rm)
                    // TODO: stuff for runtime error
                )

        }
    }

    def transCmpEqOp(op: BinOp, rd: Register, rm: Register): ListBuffer[Instr] = {
        val cond: Condition;
        op match {
            case GT => cond = GT
            case GTE => cond = GE
            case LT => cond = LT
            case LTE => cond = LE
            case Equal => cond = EQ
            case NotEqual => cond = NE
        }

        ListBuffer(
            Cmp(rd, rm),
            MovCond(cond, rd, Imm_Int(INT_FALSE))
            MovCond(cond.oppositeCmp, rd, Imm_Int(INT_FALSE))
        )
    }

    def transLgOp(op: LogicFuncs, rd: Register, rm: Register): ListBuffer[Instr] = {
        op match {
            case And => And(rd, rd, rm)
            case Or => Or(rd, rd, rm)
        }
    }
}
