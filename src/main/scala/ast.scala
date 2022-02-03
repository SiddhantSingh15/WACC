object ast{
    import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
    
    case class WaccProgram(funcs : List[Func], stat: Stat)

    case class Func(tpe : Type, ident : Ident, paramList : List[Param], stat : Stat)
    
    case class Param(tpe : Type, ident : Ident)

    sealed trait Stat
    case class Skip() extends Stat
    case class TypeAssign(tpe : Type, ident: Ident, assignRHS : AssignRHS) extends Stat
    case class AssignLR(assignLHS : AssignLHS, assignRHS : AssignRHS) extends Stat
    case class Read(assignRHS : AssignRHS) extends Stat
    case class Free(expr : Expr) extends Stat
    case class Return(expr : Expr) extends Stat
    case class Exit(expr : Expr) extends Stat
    case class Print(expr : Expr) extends Stat
    case class Println(expr : Expr) extends Stat
    case class Conditional(expr : Expr, statThen : Stat, statElse : Stat) extends Stat
    case class While(expr : Expr, stat : Stat) extends Stat
    case class SemiColon(statOne : Stat, statTwo : Stat) extends Stat

    sealed trait AssignLHS
    case class Ident(head : Char, tail : String) extends AssignLHS
    case class ArrayElem(arrayElem : ArrayElem) extends AssignLHS
    trait PairElem extends AssignLHS

    sealed trait AssignRHS
    case class Expr(expr : Expr) extends AssignRHS
    case class ArrayLiter(head : Expr, tail : List[Expr]) extends AssignRHS
    case class NewPair(exprOne : Expr, exprTwo : Expr) extends AssignRHS
    case class Call(ident : Ident, argList : ArgList) extends AssignRHS

    case class ArgList(exprs : List[Expr])

    case class Fst(fst : Expr) extends PairElem 
    case class Snd(snd : Expr) extends PairElem

    sealed trait Type 
    sealed trait BaseType extends Type with PairElemType
    case class ArrayType(tpe : Type) extends Type with PairElemType
    case class PairType(fst : PairElemType, snd : PairElemType) extends Type 

    case class Int() extends BaseType
    case class Bool() extends BaseType
    case class Char() extends BaseType
    case class String() extends BaseType

    sealed trait PairElemType
   // sealed trait BaseType extends PairElemType
  //  case class ArrayType(tpe : Type) extends PairElemType
    case class Pair() extends PairElemType

    case class UnaryOp(op : String)

    case class BinaryOp(op : String)

    case class IntLiter(sign: IntSign, digit: Digit)

    case class Digit(digit: Char)
    
    case class IntSign(sign: String)

    case class BoolLiter(bool: String)

    case class CharLiter(character: Char)

    case class StrLiter(character: List[Char])

    case class EscapedChar(chr: Char)

    case class PairLiter(string: String)


}