package frontend

import parsley.Parsley, Parsley._
import scala.collection.mutable

object Ast {
	import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
	
	case class WaccProgram(s : List[Func], stat: Stat)

	case class Func(tpe : Type, ident : Ident, paramList : ParamList, stat : Stat)
	
	case class Param(tpe : Type, ident : Ident)

	case class ParamList(params : List[Param])


	sealed trait Stat
	case object Skip extends Stat
	case class TypeAssign(tpe : Type, ident: Ident, assignRHS : AssignRHS) extends Stat
	case class AssignLR(assignLHS : AssignLHS, assignRHS : AssignRHS) extends Stat
	case class Read(assignLHS : AssignLHS) extends Stat
	case class Free(expr : Expr) extends Stat
	case class Return(expr : Expr) extends Stat
	case class Exit(expr : Expr) extends Stat
	case class Print(expr : Expr) extends Stat 
	case class Println(expr : Expr) extends Stat 
	case class If(expr : Expr, statThen : Stat, statElse : Stat) extends Stat
	case class While(expr : Expr, stat : Stat) extends Stat
	case class Begin(stat: Stat) extends Stat
	case class Colon(firstStat: Stat, secondStat: Stat) extends Stat

	sealed trait AssignLHS

	sealed trait AssignRHS {
		def getType(symbolTable : SymbolTable): Type
		var semanticErrors : mutable.ListBuffer[SemanticError]
			= mutable.ListBuffer.empty[SemanticError]
	}
	
	sealed case class Ident(string: String) extends AssignLHS with AssignRHS with Expr {
		override def getType(symbTable: SymbolTable): Type = {
			if (!symbTable.contains(this)) {
				semanticErrors += NotDeclaredFuncErr(this)
				return Any
			}
			symbTable.lookupAll(this)
		}
	}

	sealed case class ArrayElem(ident: Ident, exprList : List[Expr]) extends AssignLHS with Expr {
  		override def getType(symbTable: SymbolTable): Type = {
			  val current = ident.getType(symbTable)
				
				current match {
					case ArrayType(tpe) => return tpe
					case _ 							=> 
				}
				ident.semanticErrors += AccessDeniedErr(ident)
				ident.semanticErrors += MismatchTypesErr(ident, current, List(ArrayType(current)))
				current
		  }
	}

	sealed trait PairElem extends AssignLHS with AssignRHS

	case class ArrayLiter(list: List[Expr]) extends AssignRHS {
		override def getType(symbTable: SymbolTable): Type = {
			if (list.isEmpty) {
				return ArrayType(null)
			}

			var allExprTypes = list.map((e : Expr) => e.getType(symbTable))
			var fstType = allExprTypes.head

			if (allExprTypes.forall((t : Type) => t == fstType)) {
				return fstType
			}

			ArrayType(null)
		}
	}
	case class NewPair(exprOne : Expr, exprTwo : Expr) extends AssignRHS{
		override def getType(symbolTable: SymbolTable) : Type = {
			val typeOne = exprOne.getType(symbolTable)
			val typeTwo = exprTwo.getType(symbolTable)
			semanticErrors = exprOne.semanticErrors ++ exprTwo.semanticErrors
			var pairOneElem : PairElemType = PairElemPair //need to define PairElemPair
			if(!typeOne.isPair){
					pairOneElem = PairElemWithType(typeOne)
			}
			var pairTwoElem : PairElemType = PairElemPair
			if(!typeTwo.isPair){
					pairTwoElem = PairElemWithType(typeTwo)
			}

			return PairType(pairOneElem, pairTwoElem)
		}
	}
	case class Call(ident : Ident, argList : ArgList) extends AssignRHS {
		override def getType(symbTable: SymbolTable): Type = {
			val identType = ident.getType(symbTable)
			semanticErrors ++= symbTable.parameterMatch(ident, Some(argList))
			identType
		}
	}

	case class ArgList(exprs : List[Expr]) {
			def map(f : Expr => Expr) = ArgList(exprs.map(f))
	}

	sealed trait Type {
		def isArray : Boolean = this match {
			case ArrayType(_) => true 
			case _ 				 	  => false 
		}
		def isPair : Boolean = this match {
			case PairType(_,_) => true 
			case _             => false 
		}
	}		

  case object Any extends Type {
	override def equals(d: Any): Boolean =
		d match {
			case t: Type => true
			case _       => false
		}
  }

	sealed trait BaseType extends Type with PairElemType
	sealed case class ArrayType(tpe : Type) extends Type with PairElemType {
			override def equals(a : Any) : Boolean = 
			    a match{
					case ArrayType(null)  => true
					case ArrayType(inner) => 
						if (tpe == null) true 
						else inner == tpe
					case _ 				  => false
			}

			override def getType: Type = tpe
	}
	
	case class PairType(fst : PairElemType, snd : PairElemType) extends Type 

	case object Int extends BaseType {
		override def getType: Type = Int
	}
	case object Bool extends BaseType {
		override def getType: Type = Bool
	}
	case object CharType extends BaseType {
		override def getType: Type = CharType
	}
	case object String extends BaseType {
		override def getType: Type = String
	}

	case object Pair extends PairElemType {
		override def getType: Type = PairType(null, null)
	}

	sealed trait PairElemType extends Type {
		def getType: Type
	}

	case class PairElemWithType(t: Type) extends PairElemType {
		override def getType: Type = t
	}

	case object PairElemPair extends PairElemType {
		override def getType: Type = PairType(null, null)
	}
	case class Fst(fst : Expr) extends PairElem {
		override def getType(symbTable: SymbolTable): Type = {
			fst match {
				case Ident(_)             => val fstType  = fst.getType(symbTable)
					fstType match {
						case PairType(fst, _) => return fst.getType
						case _ => 
					}
				case _ =>
			}
			Any
		}
	}

	case class Snd(snd : Expr) extends PairElem {
		override def getType(symbTable: SymbolTable): Type = {
			snd match {
				case Ident(_) => val sndType = snd.getType(symbTable)
					sndType match {
						case PairType(_, snd) => return snd.getType
						case _ 					      =>
					}
				case _        => 
			}
			semanticErrors += InvalidPairElemErr(this)
			Any
		}
	}

	trait ParserBuilder[T] {
			val parser: Parsley[T]
			final def <#(p: Parsley[_]): Parsley[T] = parser <* p
	}
	trait ParserBuilder1[T1, R] extends ParserBuilder[T1 => R] {
			def apply(x: T1): R
			val parser = pure(apply(_))
	}
	trait ParserBuilder2[T1, T2, R] extends ParserBuilder[(T1, T2) => R] {
			def apply(x: T1, y: T2): R
			val parser = pure(apply(_, _))
	}


	sealed trait UnOp extends Expr {
		val expr: Expr
		val expected: (Type, Type)
		override def getType(symbTable: SymbolTable): Type = {
			val current = expr.getType(symbTable)
			semanticErrors = expr.semanticErrors
			if (current != expected._1) {
				semanticErrors += MismatchTypesErr(expr, current, List(expected._1))
			}
			expected._2
		}
	}

	case class Not(expr : Expr) extends UnOp {
		override val expected: (Type, Type) = (Bool, Bool)
	}
	case class Negation(expr : Expr) extends UnOp {
		override val expected: (Type, Type) = (Int, Int)
	}
	case class Len(expr : Expr) extends UnOp {
		override val expected: (Type, Type) = (ArrayType(null), Int)
		override def getType(symbTable: SymbolTable): Type = {
			val current = expr.getType(symbTable)
			semanticErrors = expr.semanticErrors
			if (!current.isArray) {
				semanticErrors += MismatchTypesErr(expr, current, List(expected._1))
			}
			expected._2
		}
	}
	
	case class Ord(expr : Expr) extends UnOp {
		override val expected: (Type, Type) = (CharType, Int)
	}

	case class Chr(expr : Expr) extends UnOp {
		override val expected: (Type, Type) = (Int, CharType)
	}

	sealed trait BinOp extends Expr {
		val exp1: Expr
		val exp2: Expr
		val expected: (List[Type], Type)

		override def getType(symbTable: SymbolTable): Type = {
			val currentExp1 = exp1.getType(symbTable)
			val currentExp2 = exp2.getType(symbTable)
			semanticErrors = exp1.semanticErrors ++ exp2.semanticErrors

			if (currentExp1 == Any || currentExp2 == Any) {
				return expected._2
			}

			if (currentExp1 != currentExp2) {
				if (!expected._1.contains(currentExp2)) {
					semanticErrors += MismatchTypesErr(exp2, currentExp2, expected._1)
				}
				if (!expected._1.contains(currentExp1)) {
					semanticErrors += MismatchTypesErr(exp1, currentExp1, expected._1)
				} else if (!expected._1.contains(exp1) && expected._1.nonEmpty) {
					semanticErrors += MismatchTypesErr(exp1, currentExp1, expected._1)
				  semanticErrors += MismatchTypesErr(exp2, currentExp2, expected._1)
				}
			}
			expected._2
		}
	}

	sealed trait MathFuncs extends BinOp {
		override val expected: (List[Type], Type) = (List(Int), Int)
	}

	sealed trait CompareFuncs extends BinOp {
		override val expected: (List[Type], Type) = (List(CharType, Int), Bool)
	}

	sealed trait LogicFuncs extends BinOp {
		override val expected: (List[Type], Type) = (List(Bool), Bool)
	}

	sealed trait EqualityFuncs extends BinOp {
		override val expected: (List[Type], Type) = (List.empty, Bool)
	}
	
	case class Mul(expr1 : Expr, expr2 : Expr) extends MathFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class Div(expr1 : Expr, expr2 : Expr) extends MathFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class Mod(expr1 : Expr, expr2 : Expr) extends MathFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class Plus(expr1 : Expr, expr2 : Expr) extends MathFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class Sub(expr1 : Expr, expr2 : Expr) extends MathFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class GT(expr1 : Expr, expr2 : Expr) extends CompareFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class GTE(expr1 : Expr, expr2 : Expr) extends CompareFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class LT(expr1 : Expr, expr2 : Expr) extends CompareFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class LTE(expr1 : Expr, expr2 : Expr) extends CompareFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class Equal(expr1 : Expr, expr2 : Expr) extends EqualityFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class NotEqual(expr1 : Expr, expr2 : Expr) extends EqualityFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class And(expr1 : Expr, expr2 : Expr) extends LogicFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}
	case class Or(expr1 : Expr, expr2 : Expr) extends LogicFuncs {
		override val exp1 = expr1
		override val exp2 = expr2
	}

	sealed trait Expr extends AssignRHS
	case class IntLiter(number: Int) extends Expr {
		override def getType(symbTable: SymbolTable): Type = Int
	}
	sealed trait BoolLiter extends Expr
	case object True extends BoolLiter {
		override def getType(symbTable: SymbolTable): Type = Bool
	}
	case object False extends BoolLiter {
		override def getType(symbTable: SymbolTable): Type = Bool
	}
	case class CharLiter(character: Char) extends Expr{
		override def getType(symbTable: SymbolTable): Type = CharType
	}
	case class StrLiter(character: String) extends Expr{
		override def getType(symbTable: SymbolTable): Type = String
	}
	case class PairLiter() extends Expr {
		override def getType(symbTable: SymbolTable): Type = Pair
	}
	// case class UnOpExpr(op: UnOp, expr: Expr) extends Expr 
	// case class BinOpExpr(expr1: Expr, op: BinOp, expr2: Expr) extends Expr {
	// 	override def getType : Type = Expr
	// }

}

