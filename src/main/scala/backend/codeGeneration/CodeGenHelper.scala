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
    // gets type for a particular rhs
    def getExprType(rhs: Expr): Type = {
        rhs match {
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

        case DerefPointer(ptr) =>
            val PointerType(inTpe) = ptr.getType(symbTable)
            inTpe

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

    def reduceRHS(rhs: AssignRHS): AssignRHS = {
        rhs match {
            case op: BinOp =>
                val expr1 = reduceRHS(op.exp1)
                val expr2 = reduceRHS(op.exp2)
                op match {
                    case mathOp: MathFuncs =>
                        (expr1, expr2, mathOp) match {
                            case (IntLiter(num1), IntLiter(num2), Div(_, _)) => 
                                num2 match {
                                    case 0 => Div(IntLiter(num1), IntLiter(num2))
                                    case _ => IntLiter(num1 / num2)
                                }
                            case (IntLiter(num1), IntLiter(num2), Mod(_, _)) => 
                                num2 match {
                                    case 0 => Mod(IntLiter(num1), IntLiter(num2))
                                    case _ => IntLiter(num1 % num2)
                                }
                            case (IntLiter(num1), IntLiter(num2), Plus(_, _)) => 
                                val sum : Long = num1.toLong + num2.toLong
                                sum match {
                                    case _ if sum != (num1 + num2).toLong => Plus(IntLiter(num1), IntLiter(num2))
                                    case _ => IntLiter(num1 + num2)
                                }
                            case (IntLiter(num1), IntLiter(num2), AST.Sub(_, _)) => 
                                val diff : Long = num1.toLong - num2.toLong
                                diff match {
                                    case _ if diff != (num1 - num2) => AST.Sub(IntLiter(num1), IntLiter(num2))
                                    case _ => IntLiter(num1 - num2)
                                }
                            case (IntLiter(num1), IntLiter(num2), Mul(_, _)) => 
                                val mult : Long = num1.toLong * num2.toLong
                                mult match {
                                    case _ if mult != (num1 * num2) => Mul(IntLiter(num1), IntLiter(num2))
                                    case _ => IntLiter(num1 * num2)
                                }
                            case _ => rhs
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
                            case _ => rhs
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
                            case (NewPair(exp11, exp12), NewPair(exp21,exp22), Equal(_, _)) =>
                                reduceRHS(AST.And(reduceRHS(Equal(exp11, exp21).asInstanceOf[AssignRHS]).asInstanceOf[Expr], 
                                reduceRHS(Equal(exp12, exp22).asInstanceOf[AssignRHS]).asInstanceOf[Expr]))
                            case (NewPair(exp11, exp12), NewPair(exp21,exp22), NotEqual(_, _)) =>
                                reduceRHS(AST.Or(reduceRHS(NotEqual(exp11, exp21).asInstanceOf[AssignRHS]).asInstanceOf[Expr], 
                                reduceRHS(NotEqual(exp12, exp22).asInstanceOf[AssignRHS]).asInstanceOf[Expr]))
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
                            case _ => rhs
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
                            case (char1: CharLiter, char2: CharLiter, GT(_, _)) =>
                                boolToBoolLiter(char1.getChar > char2.getChar)
                            case (char1: CharLiter, char2: CharLiter, GTE(_, _)) =>
                                boolToBoolLiter(char1.getChar >= char2.getChar)
                            case (char1: CharLiter, char2: CharLiter, LT(_, _)) =>
                                boolToBoolLiter(char1.getChar < char2.getChar)
                            case (char1: CharLiter, char2: CharLiter, LTE(_, _)) =>
                                boolToBoolLiter(char1.getChar <= char2.getChar)
                            case _ => rhs
                        }
                }
            case op: UnOp =>
                val redExpr = reduceRHS(op.expr)
                (op, redExpr) match {
                    case (Len(_), ArrayLiter(exprs)) => 
                        IntLiter(exprs.size)
                    case (Negation(_), IntLiter(num)) => 
                        num match {
                            case _ if num <= -2147483648 => op
                            case _ => IntLiter(-1*num)
                        }
                    case (Not(_), True) => False
                    case (Not(_), False) => True
                    case (Chr(_), IntLiter(num)) => CharLiter(NormalCharacter(num.toChar))
                    case (Ord(_), char: CharLiter) => IntLiter(char.getChar.toInt)
                    case _ => rhs
                }
            case id: Ident =>
                if (constantPropagation) {
                    return symbTable.getValue(id).getOrElse(rhs)
                }
                rhs
            case ArrayElem(id, exprList) =>
                var ident = id
                var i = 0
                while (true) {
                    var value = reduceRHS(ident)
                    if (!value.isInstanceOf[ArrayLiter]) return rhs
                    var ArrayLiter(exprs) = value
                    var IntLiter(index) = reduceRHS(exprList(i))
                    if (index < 0 || index >= exprs.size) return rhs
                    var exprValue = exprs(index)

                    exprValue match {
                        case id: Ident =>
                            ident = id
                            i += 1
                        case _ =>
                            return exprValue
                    }
                }
                rhs
            case Fst(fst) => 
                val value = reduceRHS(fst)
                if (!value.isInstanceOf[NewPair]) return rhs
                var NewPair(expr, _) = value
                if (expr == null) return rhs
                expr
            case Snd(snd) => 
                val value = reduceRHS(snd)
                if (!value.isInstanceOf[NewPair]) return rhs
                var NewPair(_, expr) = value
                if (expr == null) return rhs
                expr
            case _ =>
                rhs
        }
    }
}
