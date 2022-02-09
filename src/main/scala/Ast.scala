package compiler

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
	case class Ident(string: String) extends AssignLHS with AssignRHS with Expr
	case class ArrayElem(ident: Ident, exprList : List[Expr]) extends AssignLHS with Expr
	trait PairElem extends AssignLHS with AssignRHS

	sealed trait AssignRHS {
			def getType(symbolTable : SymbolTable): Type
			var semanticErrors : mutable.ListBuffer[SemanticError]
				= mutable.ListBuffer.empty[SemanticError]
	}
	case class ArrayLiter(list: List[Expr]) extends AssignRHS
	case class NewPair(exprOne : Expr, exprTwo : Expr) extends AssignRHS{
			override def getType(symbolTable: SymbolTable) : Type = {
					val typeOne = exprOne.getType(symbolTable)
					val typeTwo = exprTwo.getType(symbolTable)
					semanticErrors = exprOne.semanticErrors ++ exprTwo.semanticErrors
					var pairOneElem : PairElemType = PairElemPair //need to define PairElemPair
					if(!typeOne.isPair){
							pairOneElem = Pair(typeOne)
					}
					var pairTwoElem : PairElemType = PairElemPair
					if(!typeTwo.isPair){
							pairTwoElem = PairElemType(typeTwo)
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

  case object DummyType extends Type {
    override def equals(d: DummyType): Boolean =
      d match {
        case t: Type => true
        case _       => false
      }
  }

	sealed trait BaseType extends Type with PairElemType
	case class ArrayType(tpe : Type) extends Type with PairElemType {
			override def equals(d : DummyType) : Boolean = 
				d match{
					case ArrayType(null)  => true
					case ArrayType(inner) => 
						if (tpe == null) true 
						else inner == tpe
					case _ 							  => false
			}
	}
	case class PairType(fst : PairElemType, snd : PairElemType) extends Type 

	case object Int extends BaseType
	case object Bool extends BaseType
	case object CharType extends BaseType
	case object String extends BaseType

	sealed trait PairElemType {
		def getType: Type
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
			DummyType
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
			DummyType
		}
	}
		

	case object Pair extends PairElemType {
		override def getType: Type = PairType(null, null)
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
	case class Not(expr : Expr) extends UnOp
	case class Negation(expr : Expr) extends UnOp
	case class Len(expr : Expr) extends UnOp {
		override def getType(symbTable: SymbolTable): Type = {
			val current = expr.getType(symbTable)
			semanticErrors = expr.semanticErrors
			if (!current.isArray) {
				semanticErrors += MismatchTypesErr(expr, current, List(expected._1))
			}
			expected._2
		}
	}
	case class Ord(expr : Expr) extends UnOp
	case class Chr(expr : Expr) extends UnOp

	sealed trait BinOp extends Expr {
		val exp1: Expr
		val exp2: Expr
		val expected: (List[Type], Type)

		override def getType(symbTable: SymbolTable): Type = {
			val currentExp1 = exp1.getType(symbTable)
			val currentExp2 = exp2.getType(symbTable)
			semanticErrors = exp1.semanticErrors ++ exp2.semanticErrors

			if (currentExp1 == DummyType || currentExp2 == DummyType) {
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
	case class IntLiter(number: Int) extends Expr
	sealed trait BoolLiter extends Expr
	case object True extends BoolLiter 
	case object False extends BoolLiter
	case class CharLiter(character: Char) extends Expr
	case class StrLiter(character: String) extends Expr
	case class PairLiter() extends Expr
	case class UnOpExpr(op: UnOp, expr: Expr) extends Expr
	case class BinOpExpr(expr1: Expr, op: BinOp, expr2: Expr) extends Expr

}

