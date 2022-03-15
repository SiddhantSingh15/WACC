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

    def boolToBoolLiter(bool: Boolean): BoolLiter = {
        if (bool) True else False
    }

    def reduceExpr(expr: Expr): Expr = {
        expr match {
            case op: BinOp =>
                val expr1 = reduceExpr(op.exp1)
                val expr2 = reduceExpr(op.exp2)
                op match {
                    case mathOp: MathFuncs =>
                        (expr1, expr2, mathOp) match {
                            case (IntLiter(num1), IntLiter(num2), Div(_, _)) => IntLiter(num1 / num2)
                            case (IntLiter(num1), IntLiter(num2), Mod(_, _)) => IntLiter(num1 % num2)
                            case (IntLiter(num1), IntLiter(num2), Plus(_, _)) => IntLiter(num1 + num2)
                            case (IntLiter(num1), IntLiter(num2), AST.Sub(_, _)) => IntLiter(num1 - num2)
                            case (IntLiter(num1), IntLiter(num2), Mul(_, _)) => IntLiter(num1 * num2)
                            case _ => expr
                        }
                    case lgOp: LogicFuncs =>
                        (expr1, expr2, lgOp) match {
                            case (True, _, AST.And(_, _)) => expr2
                            case (_, True, AST.And(_, _)) => expr1
                            case (False, _, AST.And(_, _)) => False
                            case (_, False, AST.And(_, _)) => False
                            case (True, _, AST.Or(_, _)) => True
                            case (_, True, AST.Or(_, _)) => True
                            case (False, _, AST.Or(_, _)) => expr2
                            case (_, False, AST.Or(_, _)) => expr1
                            case _ => expr
                        }
                    case eqOp: EqualityFuncs =>
                        (expr1, expr2, eqOp) match {
                            case (IntLiter(num1), IntLiter(num2), Equal(_, _)) => 
                                boolToBoolLiter(num1 == num2)
                            case (IntLiter(num1), IntLiter(num2), NotEqual(_, _)) =>
                                boolToBoolLiter(num1 != num2)
                            case (StrLiter(str1), StrLiter(str2), Equal(_, _)) =>
                                boolToBoolLiter(str1 == str2)
                            case (StrLiter(str1), StrLiter(str2), NotEqual(_, _)) =>
                                boolToBoolLiter(str1 != str2)
                            case (CharLiter(char1), CharLiter(char2), Equal(_, _)) =>
                                boolToBoolLiter(char1 == char2)
                            case (CharLiter(char1), CharLiter(char2), NotEqual(_, _)) =>
                                boolToBoolLiter(char1 != char2)
                            case (True, True, Equal(_, _)) =>
                                True
                            case (False, False, Equal(_, _)) =>
                                True
                            case (True, False, Equal(_, _)) =>
                                False
                            case (False, True, Equal(_, _)) =>
                                False
                            case (True, True, NotEqual(_, _)) =>
                                False
                            case (False, False, NotEqual(_, _)) =>
                                False
                            case (True, False, NotEqual(_, _)) =>
                                True
                            case (False, True, NotEqual(_, _)) =>
                                True
                            case _ => expr
                        }
                    case cmpOp: CompareFuncs => 
                        (expr1, expr2, cmpOp) match {
                            case (IntLiter(num1), IntLiter(num2), GT(_, _)) => 
                                boolToBoolLiter(num1 > num2)
                            case (IntLiter(num1), IntLiter(num2), GTE(_, _)) =>
                                boolToBoolLiter(num1 >= num2)
                            case (IntLiter(num1), IntLiter(num2), LT(_, _)) =>
                                boolToBoolLiter(num1 < num2)
                            case (IntLiter(num1), IntLiter(num2), LTE(_, _)) =>
                                boolToBoolLiter(num1 <= num2)
                            case (CharLiter(_), CharLiter(_), GT(_, _)) =>
                                boolToBoolLiter(expr1.asInstanceOf[CharLiter].getChar > expr2.asInstanceOf[CharLiter].getChar)
                            case (CharLiter(_), CharLiter(_), GTE(_, _)) =>
                                boolToBoolLiter(expr1.asInstanceOf[CharLiter].getChar >= expr2.asInstanceOf[CharLiter].getChar)
                            case (CharLiter(_), CharLiter(_), LT(_, _)) =>
                                boolToBoolLiter(expr1.asInstanceOf[CharLiter].getChar < expr2.asInstanceOf[CharLiter].getChar)
                            case (CharLiter(_), CharLiter(_), LTE(_, _)) =>
                                boolToBoolLiter(expr1.asInstanceOf[CharLiter].getChar <= expr2.asInstanceOf[CharLiter].getChar)
                            case _ => expr
                        }
                }
            case op: UnOp =>
                val redExpr = reduceExpr(op.expr)
                (op, redExpr) match {
                    case (Len(_), id : Ident) => 
                        if (constantPropagation) {
                            val ArrayLiter(exprs) = symbTable.getValue(id).get.asInstanceOf[ArrayLiter]
                            return IntLiter(exprs.size)
                        }
                        expr
                    case (Negation(_), IntLiter(num)) => IntLiter(-1*num)
                    case (Not(_), True) => False
                    case (Not(_), False) => True
                    case (Chr(_), IntLiter(num)) => CharLiter(NormalCharacter(num.toChar))
                    case (Ord(_), CharLiter(_)) => IntLiter(redExpr.asInstanceOf[CharLiter].getChar.toInt)
                    case _ => expr
                }
            case id: Ident =>
                if (constantPropagation) {
                    val maybeValue = symbTable.getValue(id).get
                    maybeValue match{
                        case _: Expr => return maybeValue.asInstanceOf[Expr]
                        case _ => return expr
                    }
                }
                expr
            case ArrayElem(id, exprList) =>
                if (constantPropagation) {
                    var maybeValue = symbTable.getValue(id)
                    if (maybeValue.nonEmpty) {
                        var ArrayLiter(exprs) = maybeValue.get.asInstanceOf[ArrayLiter]
                        while (true) {
                            var i = 0
                            var IntLiter(index) = reduceExpr(exprList(i))
                            var exprValue = exprs(index)
                            exprValue match {
                                case ident: Ident =>
                                    maybeValue = symbTable.getValue(id)
                                    i += 1
                                case _ =>
                                    return exprValue
                            }
                        }
                    }  
                    expr
                } else {
                    expr
                }
            case _ =>
                expr
        }
    }
}
