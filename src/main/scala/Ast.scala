package compiler

object Ast{
    import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
    import parsley.combinator.option
    
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
    trait PairElem extends AssignLHS with AssignRHS

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
    case object CharType extends BaseType
    case object String extends BaseType

    sealed trait PairElemType
    case class Fst(fst : Expr) extends PairElem 
    case class Snd(snd : Expr) extends PairElem
   // sealed trait BaseType extends PairElemType
  //  case class ArrayType(tpe : Type) extends PairElemType
    case object Pair extends PairElemType

    sealed trait UnOp extends Expr
    case class Not(expr : Expr) extends UnOp
    case class Negation(expr : Expr) extends UnOp
    case class Len(expr : Expr) extends UnOp
    case class Ord(expr : Expr) extends UnOp
    case class Chr(expr : Expr) extends UnOp

    sealed trait BinOp extends Expr
    case class Mul(expr1 : Expr, expr2 : Expr) extends BinOp
    case class Div(expr1 : Expr, expr2 : Expr) extends BinOp
    case class Mod(expr1 : Expr, expr2 : Expr) extends BinOp
    case class Plus(expr1 : Expr, expr2 : Expr) extends BinOp
    case class Sub(expr1 : Expr, expr2 : Expr) extends BinOp
    case class GT(expr1 : Expr, expr2 : Expr) extends BinOp
    case class GTE(expr1 : Expr, expr2 : Expr) extends BinOp
    case class LT(expr1 : Expr, expr2 : Expr) extends BinOp
    case class LTE(expr1 : Expr, expr2 : Expr) extends BinOp
    case class Equal(expr1 : Expr, expr2 : Expr) extends BinOp
    case class NotEqual(expr1 : Expr, expr2 : Expr) extends BinOp
    case class And(expr1 : Expr, expr2 : Expr) extends BinOp
    case class Or(expr1 : Expr, expr2 : Expr) extends BinOp

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

    sealed abstract class Character
    case class EscapedChar(character: Char) extends Character
    case class ASCIIChar(character: Char) extends Character

}

