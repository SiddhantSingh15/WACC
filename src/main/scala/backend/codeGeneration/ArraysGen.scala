package backend.codeGeneration

import frontend.AST._
import backend.Operand._
import backend.Opcodes._
import scala.collection.mutable.ListBuffer
import backend.CodeGen._
import backend.codeGeneration.ExpressionGen._
import backend.DefinedFuncs.RuntimeErrors._
import backend.DefinedFuncs.PreDefinedFuncs._

object ArraysGen {

    def getInnerType(arrayT: Type): Type = 
        arrayT match {
            case ArrayType(innerType) => innerType
            case _                    => ???
        }

    def loadArrayElem(
        ident: Ident,
        exprs: List[Expr],
        rd: Register
    ): ListBuffer[Instr] = {
        val (instructions, isByte) = transArrayElem(ident, exprs, rd)
        instructions += Ldr(isByte, rd, rd, 0)
    }

    def storeArrayElem(
        ident: Ident,
        exprs: List[Expr],
        rd: Register
    ): ListBuffer[Instr] = {
        val freeReg = saveReg()
        val (instructions, isByte) = transArrayElem(ident, exprs, rd)
        instructions += Str(isByte, rd, freeReg, 0)
        restoreReg(freeReg)
        instructions
    }
  
    def transArrayElem(
        ident: Ident,
        exprs: List[Expr],
        rd: Register
    ): (ListBuffer[Instr], Boolean) = {
        val instructions = ListBuffer.empty[Instr]
        var (i, t) = symbTable(ident)
        val typeSize = getTypeSize(t)
        val spOffset = stackPointer - i
        instructions += Add(rd, R13_SP, Imm_Int(spOffset))
        val nextReg = saveReg()
        val innerT = getInnerType(t)

        for (expr <- exprs) {
            instructions ++= transExp(expr, nextReg)
            instructions += Ldr(rd, RegAdd(rd))
            instructions += Mov(resultRegister, nextReg)
            instructions += Mov(R1, rd)
            instructions += Bl(addRTE(ArrayBounds))
            instructions += Add(rd, rd, Imm_Int(SIZE_INT))
            
            if (isByte(innerT)) {
                instructions += Add(rd, rd, nextReg)
            } else {
                instructions += Add(rd, rd, LSL(nextReg, Imm_Int(2)))
            }
        }
        restoreReg(rd)
        (instructions, isByte(innerT))
    }

    def transArrayLiter(
        t: Type,
        arr: List[Expr],
        freeReg: Register
    ): ListBuffer[Instr] = {
        val instructions = ListBuffer.empty[Instr]
        val ArrayType(innerT) = t
        val size = arr.size

        instructions += Ldr(
            resultRegister,
            Imm_Int(SIZE_INT + size * getTypeSize(innerT))
        )
        instructions += Bl(Label("malloc"))
        instructions += Mov(freeReg, resultRegister)

        val nextFreeReg = saveReg()
        val isbyte = isByte(innerT)

        for (i <- 0 until size) {
            instructions ++= transExp(arr(i), nextFreeReg)
            if (isbyte) {
                instructions += Str(isbyte, nextFreeReg, freeReg, i + SIZE_INT)
            } else {
                instructions += Str(isbyte, nextFreeReg, freeReg, (i + 1) * SIZE_ADDR)
            }
        }

        instructions += Ldr(nextFreeReg, Load_Mem(size))
        instructions += Str(nextFreeReg, RegAdd(freeReg))
        restoreReg(nextFreeReg)
        instructions
    }
}
