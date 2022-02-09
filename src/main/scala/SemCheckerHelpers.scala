package compilers

import Ast._
import _root_.compiler.Ast

object SemCheckerHelpers {

    private def checkRead(lhs : AssignLHS, symbTable : SymbolTable) : Unit = {
			val otherType = checkType(lhs, symbTable)
			otherType match {
				case Ast.CharType | Ast.Int =>
				case _ => semanticErrors += MismatchTypesErr(lhs, otherType, List(Char, Int))				
			}
    }

    private def checkReturn(expr: Expr, symbTable: SymbolTable): Unit = {
			val expected = symbTable.getFuncRetType

			if(expected == null){
				semErrors ::= InvalidRetErr(expr)
				return        
			}

			val tpe = symbTable.getFuncRetType

			if(expected != tpe){
				semErrors ::= MismatchTypesErr(expr, tpe, List(expected))
			}
    }

    private def checkExit(expr: Expr, symbTable: SymbolTable) = {
			val tpe = checkType(expr, symbTable)

			if((tpe != null) && (tpe != Int)){
				semErrors ::= typeMismatch(expr, tpe, List(Int))
			}
    }

    private def checkIf(cond : Expr, statThen: Stat, statElse: Stat, symbTable : SymbolTable) ={
			val tpe = checkType(cond, symbTable)
			if((tpe != null) && (tpe != Bool)){
					semErrors ::= typeMismatch(cond, tpe, BoolLiter)
			}
			
			checkStat(statThen, symbTable.nextScope)
			checkStat(statElse, symbTable.nextScope)
    }

		private def checkWhile(expr: Expr, stat: Stat, symbTable: SymbolTable) = {
			val tpe = checkType(cond, symbTable)
			if((tpe != null) && (tpe != Bool)){
					semErrors ::= MismatchTypesErr(cond, tpe, BoolLiter)
			}

			checkStat(stat, symbTable)
		}

    private def checkType(expr : AssignRHS, symbTable : SymbolTable) : Type {
      val tpe = expr.tpe

    }

}
