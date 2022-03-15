package frontend

import parsley.Parsley, Parsley._
import scala.collection.mutable
import scala.io.AnsiColor._

object AST {
	import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}
	
	case class WaccProgram(s : List[Func], stats: List[Stat])

	case class Func(tpe : Type, ident : Ident, paramList : ParamList, stats : List[Stat])
	
	case class ParamList(params : List[Param])
	
	case class Param(tpe : Type, ident : Ident)

  sealed trait Expr extends AssignRHS


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
	case class If(expr : Expr, statThen : List[Stat], statElse : List[Stat]) extends Stat
	case class While(expr : Expr, stat : List[Stat]) extends Stat
	case class Begin(stat: List[Stat]) extends Stat
	// case class Colon(firstStat: Stat, secondStat: Stat) extends Stat

	sealed trait AssignLHS

	sealed trait AssignRHS {
		def getType(symbolTable : SymbolTable): Type
		var semanticErrors : mutable.ListBuffer[SemanticError]
			= mutable.ListBuffer.empty[SemanticError]
	}
	
	sealed case class Ident(string: String) extends AssignLHS with AssignRHS with Expr {
    override def toString: String = string
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
    if (current.isArray) {
      val ArrayType(t) = current
      return t
    }
    ident.semanticErrors += AccessDeniedErr(ident)
    ident.semanticErrors += 
      MismatchTypesErr(ident, current, List(ArrayType(current)))
    current
    }
	}


	case class ArrayLiter(list: List[Expr]) extends AssignRHS {
    override def toString: String = list.toString
		override def getType(symbTable: SymbolTable): Type = {
			if (list.isEmpty) {
				return ArrayType(null)
			}

			var allExprTypes = list.map((e : Expr) => e.getType(symbTable))
			var fstType = allExprTypes.head

			if (allExprTypes.forall((t : Type) => t == fstType)) {
				return ArrayType(fstType)
			}

			ArrayType(null)
		}
	}
  
	case class Call(ident : Ident, argList : ArgList) extends AssignRHS {
		override def toString: String = {
			ident.toString + "(" + argList + ")"
		}
		override def getType(symbTable: SymbolTable): Type = {
			val identType = ident.getType(symbTable)
			semanticErrors ++= symbTable.parameterMatch(ident, Some(argList))
			identType
		}
	}

	case class ArgList(exprs : List[Expr]) {
		override def toString: String = {
			exprs.mkString(", ")
		}
	}

	sealed trait Type {
		def isArray : Boolean = this match {
			case ArrayType(_) => true 
			case _ 				 	  => false 
		}
		def isPair : Boolean = this match {
			case Pair(_,_) => true 
			case _         => false 
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
    override def toString: String = {
      if (tpe != null) {
        return tpe.toString + "[]"
      }
      return "null[]"
    }
    override def equals(any : Any) : Boolean = 
      any match{
      case ArrayType(null)  => true
      case ArrayType(inner) => 
        if (tpe == null) true 
        else inner == tpe
      case _ 				  => false
    }

		override def getType: Type = tpe
	}

	case object Int extends BaseType {
    override def toString: String = "int"
		override def getType: Type = Int
	}

	case object Bool extends BaseType {
    override def toString: String = "bool"
		override def getType: Type = Bool
	}

	case object CharType extends BaseType {
    override def toString: String = "char"
		override def getType: Type = CharType
	}

	case object String extends BaseType {
    override def toString: String = "string"
		override def getType: Type = String
	}

  sealed trait PairElem extends AssignLHS with AssignRHS {
    val expr: Expr
  }

	case class NewPair(exprOne : Expr, exprTwo : Expr) extends AssignRHS{
    override def toString: String = "newpair(" + exprOne + ", " + exprTwo + ")"
		override def getType(symbolTable: SymbolTable) : Type = {
			val typeOne = exprOne.getType(symbolTable)
			val typeTwo = exprTwo.getType(symbolTable)
			semanticErrors = exprOne.semanticErrors ++ exprTwo.semanticErrors
			var pairOneElem : PairElemType = PairElemPair
			if(!typeOne.isPair){
					pairOneElem = PairElemWithType(typeOne)
			}
			var pairTwoElem : PairElemType = PairElemPair
			if(!typeTwo.isPair){
					pairTwoElem = PairElemWithType(typeTwo)
			}

			Pair(pairOneElem, pairTwoElem)
		}
	}

  sealed trait PairType extends Type
	case class Pair(fst: PairElemType, snd: PairElemType) extends PairType {
    override def toString: String = {
      if (fst == null || snd == null) {
        return "pair"
      }
      "pair(" + fst + ", " + snd + ")"
    }
    override def equals(p: Any): Boolean = 
      p match {
        case Pair(_, _) => true
        case _          => false
      }
	}

	sealed trait PairElemType extends Type {
		def getType: Type
	}

	case object PairElemPair extends PairElemType {
    override def toString: String = "pair"
		override def getType: Type = Pair(null, null)
	}

  case class PairElemWithType(tpe: Type) extends PairElemType {
    override def toString: String = tpe.toString
    override def getType: Type = tpe
  }

	case class Fst(fst : Expr) extends PairElem {
    override def toString: String = "fst: " + fst.toString
    val expr = fst
		override def getType(symbTable: SymbolTable): Type = {
			fst match {
				case Ident(_)             => val fstType = fst.getType(symbTable)
					fstType match {
						case Pair(fst, _) => return fst.getType
						case _ => 
					}
				case _                    =>
			}
      semanticErrors += InvalidPairElemErr(this)
			Any
		}
	}

	case class Snd(snd : Expr) extends PairElem {
    override def toString: String = "snd: " + snd.toString
    val expr = snd
		override def getType(symbTable: SymbolTable): Type = {
			snd match {
				case Ident(_) => val sndType = snd.getType(symbTable)
					sndType match {
						case Pair(_, snd) => return snd.getType
						case _ 					  =>
					}
				case _        => 
			}
			semanticErrors += InvalidPairElemErr(this)
			Any
		}
	}

	sealed trait UnOp extends Expr {
		val expr: Expr
		val expected: (Type, Type)
    val symbol: String
		override def getType(symbTable: SymbolTable): Type = {
			val current = expr.getType(symbTable)
			semanticErrors = expr.semanticErrors
			if (current != expected._1) {
				semanticErrors += MismatchTypesErr(expr, current, List(expected._1))
			}
			expected._2
		}
    override def toString: String = symbol + expr.toString
	}

	case class Not(expr : Expr) extends UnOp {
    val symbol = "!"
		override val expected: (Type, Type) = (Bool, Bool)
	}
	case class Negation(expr : Expr) extends UnOp {
    val symbol = "-"
		override val expected: (Type, Type) = (Int, Int)
	}
	case class Len(expr : Expr) extends UnOp {
    val symbol = "len"
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
    val symbol = "ord "
		override val expected: (Type, Type) = (CharType, Int)
	}

	case class Chr(expr : Expr) extends UnOp {
    val symbol = "chr "
		override val expected: (Type, Type) = (Int, CharType)
	}

	sealed trait BinOp extends Expr {
	  // val semanticErrs = mutable.ListBuffer.empty[SemanticError]
    	val symbol: String
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
			if (expected._1.contains(currentExp1)) {
			semanticErrors += 
				MismatchTypesErr(exp2, currentExp2, List(currentExp1))
			return expected._2
			}
			if (expected._1.contains(currentExp2)) {
			semanticErrors += 
				MismatchTypesErr(exp1, currentExp1, List(currentExp2))
			return expected._2
			}
		} else {
			if (expected._1.contains(currentExp1) && 
				expected._1.contains(currentExp2)) {
				return expected._2
				}
			if (expected._1.isEmpty) {
			return expected._2
			}
		}
      semanticErrors += MismatchTypesErr(exp1, currentExp1, expected._1)
      semanticErrors += MismatchTypesErr(exp2, currentExp2, expected._1)
			expected._2
		}
    override def toString: String = 
      exp1.toString + " " + symbol + " " + exp2.toString
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
	
	case class Mul(exp1 : Expr, exp2 : Expr) extends MathFuncs {
    val symbol = "*"
	}
	case class Div(exp1 : Expr, exp2 : Expr) extends MathFuncs {
    val symbol = "/"
	}
	case class Mod(exp1 : Expr, exp2 : Expr) extends MathFuncs {
    val symbol = "%"
	}
	case class Plus(exp1 : Expr, exp2 : Expr) extends MathFuncs {
    val symbol = "+"
	}
	case class Sub(exp1 : Expr, exp2 : Expr) extends MathFuncs {
    val symbol = "-"
	}
	case class GT(exp1 : Expr, exp2 : Expr) extends CompareFuncs {
    val symbol = ">"
	}
	case class GTE(exp1 : Expr, exp2 : Expr) extends CompareFuncs {
    val symbol = ">="
	}
	case class LT(exp1 : Expr, exp2 : Expr) extends CompareFuncs {
    val symbol = "<"
	}
	case class LTE(exp1 : Expr, exp2 : Expr) extends CompareFuncs {
    val symbol = "<="
	}
	case class Equal(exp1 : Expr, exp2 : Expr) extends EqualityFuncs {
    val symbol = "=="
	}
	case class NotEqual(exp1 : Expr, exp2 : Expr) extends EqualityFuncs {
    val symbol = "!="
	}
	case class And(exp1 : Expr, exp2 : Expr) extends LogicFuncs {
    val symbol = "&&"
	}
	case class Or(exp1 : Expr, exp2 : Expr) extends LogicFuncs {
    val symbol = "||"
	}
  
	case class IntLiter(number: Int) extends Expr {
    override def toString: String = number.toString
		override def getType(symbTable: SymbolTable): Type = Int
	}
	sealed trait BoolLiter extends Expr 
	case object True extends BoolLiter {
		override def getType(symbTable: SymbolTable): Type = Bool
	}
	case object False extends BoolLiter {
		override def getType(symbTable: SymbolTable): Type = Bool
	}
	sealed trait Character {
		override def equals(that: Any): Boolean = 
			(this, that) match {
				case (NormalCharacter(thisChar), NormalCharacter(thatChar)) =>
					thisChar == thatChar
				case (EscapeCharacter(thisChar), EscapeCharacter(thatChar)) =>
					thisChar == thatChar
				case _ =>
					false
			}
		
	}
	case class NormalCharacter(char: Char) extends Character {
		override def toString: String = char.toString
		override def equals(that: Any): Boolean = 
			that match {
				case NormalCharacter(thatChar) => 
					char == thatChar
				case _ => false
			}
	}
	case class EscapeCharacter(char: Char) extends Character {
		override def toString: String = s"\\$char"
		override def equals(that: Any): Boolean = 
			that match {
				case EscapeCharacter(thatChar) => 
					char == thatChar
				case _ => false
			}
	}
	case class CharLiter(character: Character) extends Expr{
		override def toString: String = character match {
			case NormalCharacter(_) => "\'" + character.toString() + "\'"
			case EscapeCharacter(_) => "\'" + character.toString()(1) + "\'"
		}
		def getChar: Char = this.toString.charAt(1)
		override def getType(symbTable: SymbolTable): Type = CharType
	}
	case class StrLiter(string: List[Character]) extends Expr{
    override def toString: String = string.mkString
			override def getType(symbTable: SymbolTable): Type = String
	}
	case class PairLiter() extends Expr {
    override def toString: String = "pairLit"
		override def getType(symbTable: SymbolTable): Type = Pair(null, null)
	}

  sealed trait Heap extends AssignRHS


}
