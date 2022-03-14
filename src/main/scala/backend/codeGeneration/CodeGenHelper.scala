package backend.CodeGeneration

import backend.CodeGen._
import backend.CodeGeneration.ArraysGen._
import frontend.AST._
import backend.Operand._
import backend.Opcodes._
import frontend.AST
import java.{util => ju}

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

    def getStringValue(expr: Expr): Option[String] = {
        expr match {
            case string: StrLiter => 
                Some(string.toString())
            case id: Ident =>
                val maybeValue = symbTable.getValue(id)
                if (maybeValue.isEmpty) {
                    return None
                } else {
                    return Some(maybeValue.get.asInstanceOf[String])
                }
            case _ =>
                ???            
        }
    }

    def getIntValue(expr: Expr): Option[Int] = {
        expr match {
            case ArrayElem(id, exprList) =>
                var exprs = symbTable.getValue(id).get.asInstanceOf[List[Expr]]
                while (true) {
                    var i = 0
                    var expr = exprs(getIntValue(exprList(i)).get)
                    expr match {
                        case IntLiter(number) =>
                            return Some(number)
                        case ident : Ident =>
                            exprs = symbTable.getValue(ident).get.asInstanceOf[List[Expr]]
                            i += 1
                        case _ =>
                            ???
                    }
                }
                ???
            case IntLiter(number) => 
                Some(number)
            case Negation(op) =>
                Some(-1 * getIntValue(op).get)
            case Ord(char) =>
                Some(getCharValue(char).get.toInt)
            case Len(arry) =>
                Some(arry match {
                    case ArrayElem(id, exprs) => 
                        exprs.size
                    case id: Ident =>
                        val exprs = symbTable.getValue(id).get.asInstanceOf[List[Expr]]
                        exprs.size
                    case _ => ???
                })
            case mathOp: MathFuncs => 
                Some(mathOp match {
                    case Div(exp1, exp2) => 
                        getIntValue(exp1).get / getIntValue(exp2).get
                    case Mod(exp1, exp2) => 
                        getIntValue(exp1).get % getIntValue(exp2).get 
                    case Mul(exp1, exp2) => 
                        getIntValue(exp1).get * getIntValue(exp2).get
                    case Plus(exp1, exp2) => 
                        getIntValue(exp1).get + getIntValue(exp2).get
                    case AST.Sub(exp1, exp2) => 
                        getIntValue(exp1).get - getIntValue(exp2).get
                })
            case id: Ident =>
                val maybeValue = symbTable.getValue(id)
                if (maybeValue.isEmpty) {
                    return None
                } else {
                    return Some(maybeValue.get.asInstanceOf[Int])
                }
            case _ => 
                ???
            }
    }
    def getBoolValue(expr: Expr): Option[Boolean] = {
        try {
            expr match {
                case True =>
                    Some(true)
                case False =>
                    Some(false)
                case Not(boolExpr) => 
                    Some(!getBoolValue(boolExpr).get)
                case id: Ident =>
                    val maybeValue = symbTable.getValue(id)
                    if (maybeValue.isEmpty) {
                        return None
                    } else {
                        return Some(maybeValue.get.asInstanceOf[Boolean])
                    }
                case op: BinOp =>
                    val exp1 = op.exp1
                    val exp2 = op.exp2
                    
                    val type1 = getExprType(exp1)
                    val type2 = getExprType(exp2)

                    Some(op match {
                        case eqOp: EqualityFuncs =>
                            eqOp match {
                                case Equal(exp1, exp2) => 
                                    if (type1 != type2) {
                                        false
                                    } else {
                                        type1 match {
                                            case Bool => 
                                                getBoolValue(exp1).get == getBoolValue(exp2).get
                                            case CharType => 
                                                getCharValue(exp1).get == getCharValue(exp2).get
                                            case Int => 
                                                getIntValue(exp1).get == getIntValue(exp2).get
                                            case String =>
                                                getStringValue(exp1).get == getStringValue(exp2).get
                                            case _ =>
                                                ???
                                        }
                                    }
                                case NotEqual(exp1, exp2) => 
                                    if (type1 != type2) {
                                        true
                                    } else {
                                        type1 match {
                                            case Bool => 
                                                getBoolValue(exp1).get != getBoolValue(exp2).get
                                            case CharType => 
                                                getCharValue(exp1).get != getCharValue(exp2).get
                                            case Int => 
                                                getIntValue(exp1).get != getIntValue(exp2).get
                                            case String =>
                                                getStringValue(exp1).get != getStringValue(exp2).get
                                            case _ =>
                                                ???
                                        }
                                    }
                            }
                        case lgOp: LogicFuncs =>
                            lgOp match {
                                case AST.And(exp1, exp2) => 
                                    getBoolValue(exp1).get && getBoolValue(exp2).get
                                case AST.Or(exp1, exp2) => 
                                    getBoolValue(exp1).get || getBoolValue(exp2).get
                            }
                        case cmpOp: CompareFuncs =>
                            cmpOp match {
                                case GT(exp1, exp2) => 
                                    val exprType = getExprType(exp1)
                                    exprType match {
                                        case Int =>
                                            getIntValue(exp1).get > getIntValue(exp2).get
                                        case CharType => 
                                            getCharValue(exp1).get > getCharValue(exp2).get
                                        case _ =>
                                            false
                                    }
                                case GTE(exp1, exp2) => 
                                    val exprType = getExprType(exp1)
                                    exprType match {
                                        case Int =>
                                            getIntValue(exp1).get >= getIntValue(exp2).get
                                        case CharType => 
                                            getCharValue(exp1).get >= getCharValue(exp2).get
                                        case _ =>
                                            false
                                    }
                                case LT(exp1, exp2) => 
                                    val exprType = getExprType(exp1)
                                    exprType match {
                                        case Int =>
                                            getIntValue(exp1).get < getIntValue(exp2).get
                                        case CharType => 
                                            getCharValue(exp1).get < getCharValue(exp2).get
                                        case _ =>
                                            false
                                    }
                                case LTE(exp1, exp2) => 
                                    val exprType = getExprType(exp1)
                                    exprType match {
                                        case Int =>
                                            getIntValue(exp1).get <= getIntValue(exp2).get
                                        case CharType => 
                                            getCharValue(exp1).get <= getCharValue(exp2).get
                                        case _ =>
                                            ???
                                    }
                            }
                        case _ =>
                            ???
                    })
                case _ =>
                    ???      
            }
        }
        catch {
            case _: ju.NoSuchElementException => None
        }
    }

    def getCharValue(expr: Expr): Option[Char] = {
        expr match {
            case character: CharLiter => 
                Some(character.toString.charAt(1))
            case Chr(intExpr) => 
                Some(getIntValue(intExpr).get.toChar)
            case id: Ident =>
                val maybeValue = symbTable.getValue(id)
                if (maybeValue.isEmpty) {
                    return None
                } else {
                    return Some(maybeValue.get.asInstanceOf[Char])
                }
            case _ =>
                ???
        }
    }
}
