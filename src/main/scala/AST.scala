object ast{
    import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
    
    case class WaccProgram(funcs : List[Func], stat: Stat)

    case class Func(tpe : Type, ident : Ident, paramList : List[Param], stat : Stat)
    
    case class Param(tpe : Type, ident : Ident)

    case class Stat(tpe : Type = null, ident : Ident = null, 
                    assignREqual : AssignRHS = null, assignLEqual : AssignLHS = null,
                    assignLRead : AssignLHS = null, exprFree : Expr = null, 
                    exprReturn : Expr = null, exprExit : Expr = null, exprPrint : Expr = null,
                    exprPrintln : Expr = null, exprIf : Expr = null, statIf1 : Stat = null,
                    statIf2 : Stat = null, exprWhile : Expr = null, statWhile : Stat = null,
                    statBeginEnd : Stat = null, stat1Semi : Stat = null, stat2Semi : Stat = null)


    case class AssignLHS(ident : Ident, arrayElem : ArrayElem, pairElem : PairElem)

    case class AssignRHS(expr : Expr = null, arrayLiter : ArridenayLiter = null, 
                        exprEqualLeft : Expr = null, exprEqualRight : Expr = null,
                        pairElem : PairElem = null, ident : Ident = null, 
                        argList : ArgList = null)

    
    case class ArgList(exprs : List[Expr])

    case class PairElem(fst : Expr, snd : Expr)

    case class Type(base: BaseType, array: ArrayType, pair: PairType)

    case class BaseType(bType: String)

    case class ArrayType(tpe: Type)

    case class UnaryOp(op : String)

    case class BinaryOp(op : String)

    case class Ident(head : Char, tail : String)

    case class ArrayElem(ident: Ident, expr: Expr)

    case class IntLiter(sign: IntSign, digit: Digit)

    case class Digit(digit: Char)
    
    case class IntSign(sign: String)

    case class BoolLiter(bool: String)

    case class CharLiter(character: Char)

    case class StrLiter(character: List[Char])

    case class EscapedChar(chr: Char)

    case class PairLiter(string: String)

    case class ArrayLiter(head : Expr, tail : List[Expr])

}