package backend.CodeGeneration

import backend.CodeGen._
import backend.CodeGeneration.ArraysGen._
import frontend.AST._
import backend.Operand._
import backend.Opcodes._
import frontend.AST

object CodeGenHelper {

    val SIZE_ADDR = 4
    val SIZE_PAIR = SIZE_ADDR
    val SIZE_ARR = SIZE_ADDR
    val SIZE_INT = 4
    val SIZE_CHAR = 1
    val SIZE_BOOL = 1
    val SIZE_STR = 4

    val MAX_IMM_INT = 1024

    private val ERROR = -1

    def typeOf(tpe: Type): Type = tpe match {
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

    def incrementSP(toInc: Int): Unit = {
        if (toInc == 0) {
        return
        }
        var currToInc = toInc
        while (currToInc > MAX_IMM_INT) {
        currToInc -= MAX_IMM_INT
        currInstructions.add(backend.Opcodes.Add(R13_SP, R13_SP, Imm_Int(MAX_IMM_INT)))
        }
        currInstructions.add(backend.Opcodes.Add(R13_SP, R13_SP, Imm_Int(currToInc)))
    }

    def decrementSP(toDec: Int): Unit = {
        if (toDec == 0) {
        return
        }
        var currToDec = toDec
        while (currToDec > MAX_IMM_INT) {
        currToDec -= MAX_IMM_INT
        currInstructions.add(backend.Opcodes.Sub(R13_SP, R13_SP, Imm_Int(MAX_IMM_INT)))
        }
        currInstructions.add(backend.Opcodes.Sub(R13_SP, R13_SP, Imm_Int(currToDec)))
    }

    def getIntValue(expr: Expr): Int = {
        expr match {
            case IntLiter(number) => 
                number
            case Negation(op) =>
                -1 * getIntValue(op)
            case Ord(char) =>
                getCharValue(char).toInt
            case Len(arry) =>
                val ArrayElem(id, exprs) = arry
                exprs.size
            case mathOp: MathFuncs => 
                mathOp match {
                    case Div(exp1, exp2) => 
                        getIntValue(exp1) / getIntValue(exp2)
                    case Mod(exp1, exp2) => 
                        getIntValue(exp1) % getIntValue(exp2) 
                    case Mul(exp1, exp2) => 
                        getIntValue(exp1) * getIntValue(exp2)
                    case Plus(exp1, exp2) => 
                        getIntValue(exp1) + getIntValue(exp2)
                    case AST.Sub(exp1, exp2) => 
                        getIntValue(exp1) - getIntValue(exp2)
                }
            case id: Ident =>
                symbTable.getValue(id).get.asInstanceOf[Int]
            case _ => 
                0
        }
    }

    def getBoolValue(expr: Expr): Boolean = {
        expr match {
            case True =>
                true
            case False =>
                false
            case Not(boolExpr) => 
                !getBoolValue(boolExpr)
            case eqOp: EqualityFuncs =>
                eqOp match {
                    case Equal(exp1, exp2) => 
                        getBoolValue(exp1) == getBoolValue(exp2)
                    case NotEqual(exp1, exp2) => 
                        getBoolValue(exp1) != getBoolValue(exp2)
                }
            case lgOp: LogicFuncs =>
                lgOp match {
                    case AST.And(exp1, exp2) => 
                        getBoolValue(exp1) && getBoolValue(exp2)
                    case AST.Or(exp1, exp2) => 
                        getBoolValue(exp1) || getBoolValue(exp2)
                }
            case cmpOp: CompareFuncs =>
                cmpOp match {
                    case GT(exp1, exp2) => 
                        val exprType = getExprType(exp1)
                        exprType match {
                            case Int =>
                                getIntValue(exp1) > getIntValue(exp2)
                            case CharType => 
                                getCharValue(exp1) > getCharValue(exp2)
                            case _ =>
                                false
                        }
                    case GTE(exp1, exp2) => 
                        val exprType = getExprType(exp1)
                        exprType match {
                            case Int =>
                                getIntValue(exp1) >= getIntValue(exp2)
                            case CharType => 
                                getCharValue(exp1) >= getCharValue(exp2)
                            case _ =>
                                false
                        }
                    case LT(exp1, exp2) => 
                        val exprType = getExprType(exp1)
                        exprType match {
                            case Int =>
                                getIntValue(exp1) < getIntValue(exp2)
                            case CharType => 
                                getCharValue(exp1) < getCharValue(exp2)
                            case _ =>
                                false
                        }
                    case LTE(exp1, exp2) => 
                        val exprType = getExprType(exp1)
                        exprType match {
                            case Int =>
                                getIntValue(exp1) <= getIntValue(exp2)
                            case CharType => 
                                getCharValue(exp1) <= getCharValue(exp2)
                            case _ =>
                                false
                        }
                }
            case id: Ident =>
                symbTable.getValue(id).get.asInstanceOf[Boolean]
            case _ =>
                false      
        }
    }

    def getCharValue(expr: Expr): Char = {
        expr match {
            case character: CharLiter => 
                character.toString.charAt(1)
            case Chr(intExpr) => 
                getIntValue(intExpr).toChar
            case id: Ident =>
                symbTable.getValue(id).get.asInstanceOf[Char]
            case _ =>
                'z'
        }
    }
}
