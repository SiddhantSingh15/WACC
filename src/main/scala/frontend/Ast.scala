package frontend

import parsley.Parsley, Parsley._
import scala.collection.mutable
import parsley.implicits.zipped.LazyZipped2

object Ast {
	import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}

	type Row = Int 
	type Col = Int

	def printPosition(pos : (Row, Col)): String = pos match {
		case (r : Int, c : Int) => "line: " + line + ", column: " + c
	}

	case class WaccProgram(funcs : List[Func], stat: Stat){
		
		def printStats(stat : Stat, str : String) : StringBuilder = {
			val sb = new StringBuilder()
			sb.append(stat)
			// stat match {
				// 
				// case Seq(stats) => 
					// for(s <- stats){
						// sb.append(s"$str $s\n")
					// }
				// case _ => sb.append(stat)
			// }
		}
		
		
		override def toString : String = {
			val sb = new StringBuilder()
			for(func <- funcs){
				val Func(tpe, ident, pls, stat) = func 
				sb.append(s"function $ident, $pls \n")
				sb.append(printStats(stat, "\t"))
				sb.append("end of function")
			}
			sb.append("main\n")
			sb.append(printStats(stat, ""))
			sb.append("end of main\n")
			return sb.toString()
		}
	}

	case class Func(tpe : Type, ident : Ident, paramList : ParamList, stat : Stat)
	
	case class Param(tpe : Type, ident : Ident)

	case class ParamList(params : List[Param])

  sealed trait Expr extends AssignRHS {
	override def map[B >: AssignRHS](f: Expr => Expr): Expr = this
    val identPresent = false 
  }

	sealed trait Stat{
		def map(f : Expr => Expr) : Stat = this
	}
	case object Skip extends Stat
	case class TypeAssign(tpe : Type, ident: Ident, assignRHS : AssignRHS) extends Stat
	case class AssignLR(assignLHS : AssignLHS, assignRHS : AssignRHS) extends Stat
	case class Read(assignLHS : AssignLHS) extends Stat
	case class Free(expr : Expr) extends Stat {
		override def map(f : Expr => Expr) = Free(f(expr))
	}
	case class Return(expr : Expr) extends Stat {
		override def map(f : Expr => Expr) = Return(f(expr))
	}
	case class Exit(expr : Expr) extends Stat {
		override def map(f : Expr => Expr) = Exit(f(expr))
	}
	case class Print(expr : Expr) extends Stat {
		override def map(f : Expr => Expr) = Print(f(expr))
	}
	case class Println(expr : Expr) extends Stat {
		override def map(f : Expr => Expr) = Println(f(expr))
	}
	case class If(expr : Expr, statThen : Stat, statElse : Stat) extends Stat
	case class While(expr : Expr, stat : Stat) extends Stat
	case class Begin(stat: Stat) extends Stat
	case class Colon(firstStat: Stat, secondStat: Stat) extends Stat

	sealed trait AssignLHS

	sealed trait AssignRHS {

		val pos : (Row, Col)

		def getType(symbolTable : SymbolTable): Type
		var semanticErrors : mutable.ListBuffer[SemanticError]
			= mutable.ListBuffer.empty[SemanticError]
		
		def map[B >: AssignRHS](f : Expr => Expr): B = this
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

	object Ident {
    	def apply(str: Parsley[String]): Parsley[Ident] =
     		 str.map((s: String) => (p: (Row, Col)) => Ident(s, p)) <*> pos
	}


	sealed case class ArrayElem(ident: Ident, exprList : List[Expr]) extends AssignLHS with Expr {
  		override def getType(symbTable: SymbolTable): Type = {
			  val current = ident.getType(symbTable)
        if (current.isArray) {
          val ArrayType(t) = current
          return t
        }
				ident.semanticErrors += AccessDeniedErr(ident)
				ident.semanticErrors += MismatchTypesErr(ident, current, List(ArrayType(current)))
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
  
	// case class Call(ident : Ident, argList : Option[ArgList])(val pos : (Row, Col)) extends AssignRHS {
		// override def toString: String = {
			// ident.toString + "(" + argList + ")"
		// }
		// override def getType(symbTable: SymbolTable): Type = {
			// val identType = ident.getType(symbTable)
			// semanticErrors ++= symbTable.parameterMatch(ident, argList)
			// identType
		// }
		// override def map[B >: AssignRHS](f : Expr => Expr) = 
			// Call(ident, argList.map(arg => arg.map(f)))(pos)
	// }
	// object Call {
		// def apply(id: Parsley[Ident], args: Parsley[Option[ArgList]]):Parsley[Call] =
			// pos <**> (id, args).lazyZipped(Call(_, _) _)
	// }

	case class ArgList(exprs : List[Expr]) {
		override def toString: String = {
			exprs.mkString(", ")
		}
		def map(f : Expr => Expr) = ArgList(exprs.map(f))
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
    override def toString: String = "char"
		override def getType: Type = String
	}

  sealed trait PairElem extends AssignLHS with AssignRHS {
    val expr: Expr
  }

	case class NewPair(exprOne : Expr, exprTwo : Expr, pos : (Row, Col)) extends AssignRHS{
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
	object NewPair{		
		def apply(fst : Parsley[Expr], snd : Parsley[Expr]) : Parsley[NewPair] =
			pos <**> (fst, snd).lazyZipped(NewPair(_, _) _)
			//((fst, snd)_).map((fst: Expr, snd: Expr) =>
			//        (p: (Row, Col)) => NewPair(fst, snd, p)
			//		) <*> pos
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
	case class Fst(fst : Expr, pos : (Row, Col)) extends PairElem {
    override def toString: String = "fst: " + fst.toString

	override def map[B >: AssignRHS](f: Expr => Expr) = Fst(f(expr), pos)
    val expr = fst
		override def getType(symbTable: SymbolTable): Type = {
			fst match {
				case Ident(_, _)             => val fstType = fst.getType(symbTable)
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

	object Fst {
		def apply(e : Parsley[Expr]) : Parsley[Fst] = 
			pos <**> e.map((e : Expr) => (p: (Row, Col)) => Fst(e, p))
	}

	case class Snd(snd : Expr, pos : (Row, Col)) extends PairElem {
    	override def toString: String = "snd: " + snd.toString

		override def map[B >: AssignRHS](f: Expr => Expr) = Snd(f(snd), pos)
    	val expr = snd
		override def getType(symbTable: SymbolTable): Type = {
			snd match {
				case Ident(_, _) => val sndType = snd.getType(symbTable)
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

	object Snd {
		def apply(e : Parsley[Expr]) : Parsley[Snd] =
			pos <**> e.map((e : Expr) => (p : (Row, Col)) => Snd(e, p))
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

	case class Not(expr : Expr, pos : (Row, Col)) extends UnOp {
    	val symbol = "!"
		override val expected: (Type, Type) = (Bool, Bool)

		override def map[B >: AssignRHS](f : Expr => Expr) = Not(f(expr), pos)
	}
	object Not {
		def apply(op : Parsley[_]): Parsley[Expr => Expr] =
			pos.map((p : (Row, Col)) => (e : Expr) => Not(e, p)) <* op
	}
	case class Negation(expr : Expr, pos : (Row, Col)) extends UnOp {
    	val symbol = "-"
		override val expected: (Type, Type) = (Int, Int)

		override def map[B >: AssignRHS](f : Expr => Expr) = Negation(f(expr), pos)
	}
	case class Len(expr : Expr, pos : (Row, Col)) extends UnOp {
    	val symbol = "len"
		override val expected: (Type, Type) = (ArrayType(null), Int)

		override def map[B >: AssignRHS](f: Expr => Expr) = Len(f(expr), pos)
		override def getType(symbTable: SymbolTable): Type = {
			val current = expr.getType(symbTable)
			semanticErrors = expr.semanticErrors
			if (!current.isArray) {
				semanticErrors += MismatchTypesErr(expr, current, List(expected._1))
			}
			expected._2
		}
	}

	object Len {
		def apply(op : Parsley[_]) : Parsley[Expr => Expr] =
			pos.map((p: (Row, Col)) => (e: Expr) => Len(e, p)) <* op
	}
	
	case class Ord(expr : Expr, pos : (Row, Col)) extends UnOp {
   		val symbol = "ord "
		override val expected: (Type, Type) = (CharType, Int)

		override def map[B >: AssignRHS](f : Expr => Expr) = Ord(f(expr), pos)
	}

	object Ord {
    	def apply(op: Parsley[_]): Parsley[Expr => Expr] =
      		pos.map((p: (Row, Col)) => (e: Expr) => Ord(e, p)) <* op
  }

	case class Chr(expr : Expr, pos : (Row, Col)) extends UnOp {
    	val symbol = "chr "
		override val expected: (Type, Type) = (Int, CharType)

		override def map[B >: AssignRHS](f : Expr => Expr) = Chr(f(expr), pos)
	}

	object Chr {
		def apply(op : Parsley[_]): Parsley[Expr => Expr] = 
			pos.map((p : (Row, Col)) => (e : Expr) => Chr(e, p))
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
          semanticErrors += MismatchTypesErr(exp2, currentExp2, List(currentExp1))
          return expected._2
        }
        if (expected._1.contains(currentExp2)) {
          semanticErrors += MismatchTypesErr(exp1, currentExp1, List(currentExp2))
          return expected._2
        }
      } else {
        if (expected._1.contains(currentExp1) && expected._1.contains(currentExp2)) {
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
    override def toString: String = exp1.toString + " " + symbol + " " + exp2.toString
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
	
	case class Mul(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends MathFuncs {
    val symbol = "*"
	
	override def map[B >: AssignRHS](f: Expr => Expr) =
      Mul(f(exp1), f(exp2), pos)
	}
	object Mul {
		def apply(op : Parsley[_]) : Parsley[(Expr, Expr) => Expr] = 
			pos.map((p : (Row, Col)) => (l : Expr, r : Expr) => Mul(l,r,p)) <* op
	}
	case class Div(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends MathFuncs {
    	val symbol = "/"
		override def map[B >: AssignRHS](f: Expr => Expr) =
			Div(f(exp1), f(exp2), pos)
	}

	object Div {
		def apply(op : Parsley[_]) : Parsley[(Expr, Expr) => Expr] =
			pos.map((p: (Row, Col)) => (l: Expr, r: Expr) => Div(l, r, p)) <* op
	}
	case class Mod(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends MathFuncs {
    	val symbol = "%"
		override def map[B >: AssignRHS](f : Expr => Expr) =
			Mod(f(exp1), f(exp2), pos)
	}

	object Mod {
    	def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      		pos.map((p: (Row, Col)) => (l: Expr, r: Expr) => Mod(l, r, p)) <* op
  }
	case class Plus(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends MathFuncs {
    	val symbol = "+"
		override def map[B >: AssignRHS](f : Expr => Expr) = 
			Plus(f(exp1), f(exp2), pos)
	}

	object Plus {
    	def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      		pos.map((p: (Row, Col)) => (l: Expr, r: Expr) => Plus(l, r, p)) <* op
  }
	case class Sub(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends MathFuncs {
    	val symbol = "-"
		override def map[B >: AssignRHS](f: Expr => Expr) =
      		Sub(f(exp1), f(exp2), pos)
	}

	object Sub {
    	def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      		pos.map((p: (Row, Col)) => (l: Expr, r: Expr) => Sub(l, r, p)) <* op
  }

	case class GT(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends CompareFuncs {
    val symbol = ">"
	override def map[B >: AssignRHS](f: Expr => Expr) =
      GT(f(exp1), f(exp2), pos)
	}

	object GT {
		def apply(op: Parsley[_]): Parsley[(Expr, Expr) => Expr] =
      		pos.map((p: (Row, Col)) => (l: Expr, r: Expr) => GT(l, r, p)) <* op
	}
	case class GTE(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends CompareFuncs {
    	val symbol = ">="
		override def map[B >: AssignRHS](f: Expr => Expr) =
      		GTE(f(exp1), f(exp2), pos)
	}

	object GTE {
		def apply(op : Parsley[_]) : Parsley[(Expr, Expr) => Expr] = 
			pos.map((p : (Row, Col)) => (l : Expr, r : Expr) => GTE(l,r,p)) <* op
	}
	case class LT(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends CompareFuncs {
    	val symbol = "<"
		override def map[B >: AssignRHS](f: Expr => Expr) =
      		LT(f(exp1), f(exp2), pos)
	}

	object LT {
		def apply(op : Parsley[_]) : Parsley[(Expr, Expr) => Expr] = 
			pos.map((p : (Row, Col)) => (l : Expr, r : Expr) => LT(l,r,p)) <* op
	}
	case class LTE(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends CompareFuncs {
    	val symbol = "<="
		override def map[B >: AssignRHS](f: Expr => Expr) =
      		LTE(f(exp1), f(exp2), pos)
	}

	object LTE {
		def apply(op : Parsley[_]) : Parsley[(Expr, Expr) => Expr] = 
			pos.map((p : (Row, Col)) => (l : Expr, r : Expr) => LTE(l,r,p)) <* op
	}
	case class Equal(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends EqualityFuncs {
    	val symbol = "=="
		override def map[B >: AssignRHS](f: Expr => Expr) =
      		Equal(f(exp1), f(exp2), pos)
	}

	object Equal {
		def apply(op : Parsley[_]) : Parsley[(Expr, Expr) => Expr] = 
			pos.map((p : (Row, Col)) => (l : Expr, r : Expr) => Equal(l,r,p)) <* op
	}
	case class NotEqual(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends EqualityFuncs {
    	val symbol = "!="
		override def map[B >: AssignRHS](f: Expr => Expr) =
      		NotEqual(f(exp1), f(exp2), pos)
	}

	object NotEqual {
		def apply(op : Parsley[_]) : Parsley[(Expr, Expr) => Expr] = 
			pos.map((p : (Row, Col)) => (l : Expr, r : Expr) => NotEqual(l,r,p)) <* op
	}
	case class And(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends LogicFuncs {
    	val symbol = "&&"
		override def map[B >: AssignRHS](f: Expr => Expr) =
      		And(f(exp1), f(exp2), pos)
	}
	
	object And {
		def apply(op : Parsley[_]) : Parsley[(Expr, Expr) => Expr] = 
			pos.map((p : (Row, Col)) => (l : Expr, r : Expr) => And(l,r,p)) <* op
	}
	case class Or(exp1 : Expr, exp2 : Expr, pos : (Row, Col)) extends LogicFuncs {
    	val symbol = "||"
		override def map[B >: AssignRHS](f: Expr => Expr) =
      		Or(f(exp1), f(exp2), pos)
	}

	object Or {
		def apply(op : Parsley[_]) : Parsley[(Expr, Expr) => Expr] = 
			pos.map((p : (Row, Col)) => (l : Expr, r : Expr) => Or(l,r,p)) <* op
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
	case class CharLiter(character: Char) extends Expr{
    override def toString: String = "\'" + character + "\'"
		override def getType(symbTable: SymbolTable): Type = CharType
	}
	case class StrLiter(string: String) extends Expr{
    override def toString: String = string
		override def getType(symbTable: SymbolTable): Type = String
	}
	case class PairLiter() extends Expr {
    override def toString: String = "pairLit"
		override def getType(symbTable: SymbolTable): Type = Pair(null, null)
	}
	// case class UnOpExpr(op: UnOp, expr: Expr) extends Expr 
	// case class BinOpExpr(expr1: Expr, op: BinOp, expr2: Expr) extends Expr {
	// 	override def getType : Type = Expr
	// }

}

