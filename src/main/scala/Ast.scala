package compiler

object Ast{
    import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
    
    case class WaccProgram(s : List[Func], stat: Stat)

    case class Func(tpe : Type, ident : Ident, paramList : ParamList, stat : Stat)
    
    case class Param(tpe : Type, ident : Ident)

    case class ParamList(params : List[Param])


    sealed trait Stat
    case object Skip extends Stat
    case class TypeAssign(tpe : Type, ident: Ident, assignRHS : AssignRHS) extends Stat
    case class AssignLR(assignLHS : AssignLHS, assignRHS : AssignRHS) extends Stat
    case class Read(assignRHS : AssignLHS) extends Stat
    case class Free(expr : Expr) extends Stat 
    // { override def map(func: Expr => Expr) = Free(func(expr)) }
    case class Return(expr : Expr) extends Stat 
    // { override def map(func: Expr => Expr) = Return(func(expr)) }
    case class Exit(expr : Expr) extends Stat 
    // { override def map(func: Expr => Expr) = Exit(func(expr)) }
    case class Print(expr : Expr) extends Stat 
    // { override def map(func: Expr => Expr) = Print(func(expr)) }
    case class Println(expr : Expr) extends Stat 
    // { override def map(func: Expr => Expr) = Println(func(expr)) }
    case class If(expr : Expr, statThen : Stat, statElse : Stat) extends Stat
    case class While(expr : Expr, stat : Stat) extends Stat
    case class Begin(stat: Stat) extends Stat
    case class Colon(firstStat: Stat, secondStat: Stat) extends Stat

    sealed trait AssignLHS
    case class Ident(string: String) extends AssignLHS with Expr
    case class ArrayElem(ident: Ident, exprList : List[Expr]) extends AssignLHS with Expr
    trait PairElem extends AssignLHS

    sealed trait AssignRHS
    case class ArrayLiter(list: List[Expr]) extends AssignRHS
    case class NewPair(exprOne : Expr, exprTwo : Expr) extends AssignRHS
    case class Call(ident : Ident, argList : ArgList) extends AssignRHS

    case class ArgList(exprs : List[Expr])

    sealed trait Type 
    sealed trait BaseType extends Type with PairElemType
    case class ArrayType(tpe : Type) extends Type with PairElemType
    case class PairType(fst : PairElemType, snd : PairElemType) extends Type 

    case object Int extends BaseType
    case object Bool extends BaseType
    case object Char extends BaseType
    case object String extends BaseType

    sealed trait PairElemType
    case class Fst(fst : Expr) extends PairElem 
    case class Snd(snd : Expr) extends PairElem
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

    sealed trait Expr extends AssignRHS
    case class IntLiter(sign: IntSign, digits: List[Digit]) extends Expr // Should be a list of digits
    sealed trait BoolLiter extends Expr
    case object True extends BoolLiter 
    case object False extends BoolLiter
    case class CharLiter(character: Character) extends Expr
    case class StrLiter(character: List[Character]) extends Expr
    case class PairLiter() extends Expr
    case class UnOpExpr(op: UnOp, expr: Expr) extends Expr
    case class BinOpExpr(expr1: Expr, op: BinOp, expr2: Expr) extends Expr

    case class Digit(digit: Int)
    
    sealed trait IntSign
    case object Pos extends IntSign
    case object Neg extends IntSign

    case class Character(character: Char)

}

