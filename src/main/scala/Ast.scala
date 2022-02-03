object Ast{
    import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
    
    case class WaccProgram(funcs : List[Func], stat: Stat)

    case class Func(tpe : Type, ident : Ident, paramList : List[Param], stat : Stat)
    
    case class Param(tpe : Type, ident : Ident)

    sealed trait Stat
    case object Skip extends Stat
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

    case object Int extends BaseType
    case object Bool extends BaseType
    case object Char extends BaseType
    case object String extends BaseType

    sealed trait PairElemType
   // sealed trait BaseType extends PairElemType
  //  case class ArrayType(tpe : Type) extends PairElemType
    case object Pair extends PairElemType

    sealed trait UnOp
    case object Not extends UnOp
    case object Negation extends UnOp
    case object Len extends UnOp
    case object Ord extends UnOp
    case object Chr extends UnOp

    sealed trait BinOp
    case object Mul extends BinOp
    case object Div extends BinOp
    case object Mod extends BinOp
    case object Plus extends BinOp
    case object Sub extends BinOp
    case object GT extends BinOp
    case object GTE extends BinOp
    case object LT extends BinOp
    case object LTE extends BinOp
    case object Equal extends BinOp
    case object NotEqual extends BinOp
    case object And extends BinOp
    case object Or extends BinOp

    case class IntLiter(sign: IntSign, digit: Digit)

    case class Digit(digit: Char)
    
    sealed trait IntSign
    case object Pos extends IntSign
    case object Neg extends IntSign

    case class BoolLiter(bool: String)

    case class CharLiter(character: Char)

    case class StrLiter(character: List[Char])

    case class EscapedChar(chr: Char)

    case class PairLiter(string: String)


}