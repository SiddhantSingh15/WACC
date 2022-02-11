package frontend

import Ast._
import scala.collection.mutable

object SemanticChecker {
  
  private var semanticErrors = mutable.ListBuffer.empty[SemanticError]

  def checkProgram(prog: WaccProgram): (SymbolTable, mutable.ListBuffer[SemanticError]) = {
      val WaccProgram(s, stat) = prog
      val globSymTable: SymbolTable = SymbolTable(null, null, new mutable.HashMap[Ident, Info])
      val globFuncs = s.map(convertFuncType)
      semanticErrors ++= globSymTable.addFunctions(globFuncs)

      for (func <- s) {
        checkFunc(func, globSymTable.nextScope(func.ident))
      }

      checkStat(stat, globSymTable)
      (globSymTable, semanticErrors)
	}

  def checkFunc(func: Func, symbTable: SymbolTable): Unit = {
    val Func(tpe, ident, paramList, stat) = func 
    paramList match { 
      case ParamList(pList) =>
      symbTable.addVariables(pList.map((param: Param) => (param.ident, param.tpe)))
      case _ =>   
    }
    checkStat(stat, symbTable)
  }

  def checkStat(stat: Stat, symbTable: SymbolTable): Unit = 
    stat match {
      case Read(assignLHS)                => checkRead(assignLHS, symbTable)
      case Free(expr)                     => checkFree(expr, symbTable)
      case Return(expr)                   => checkReturn(expr, symbTable)
      case Exit(expr)                     => checkExit(expr, symbTable)
      case Print(expr)                    => checkType(expr, symbTable)
      case Println(expr)                  => checkType(expr, symbTable)
      case If(expr, statThen, statElse)   => checkIf(expr, statThen, statElse, symbTable)
      case While(expr, stat)              => checkWhile(expr, stat, symbTable)
      case Begin(stat)                    => checkStat(stat, symbTable.nextScope)
      case AssignLR(assignLHS, assignRHS) => checkAssign(assignLHS, assignRHS, symbTable)
      case Colon(fstStat, sndStat)        => checkColon(fstStat, sndStat, symbTable)
      case TypeAssign(t, ident, rhs)      => checkTypeAssign(t, ident, rhs, symbTable)
      case _                              => 

  }
  
  private def convertFuncType(f: Func): (Ident, Info) = {
    val Func(tpe, ident, ParamList(pList), _) = f
    val pTypes = pList.map(_.tpe)
    (ident, Info(tpe, Some(pTypes)))
  }

  private def checkColon(fstStat: Stat, sndStat: Stat, symbTable: SymbolTable): Unit = {
    checkStat(fstStat, symbTable)
    checkStat(sndStat, symbTable)
  }

  private def checkTypeAssign(tpe: Type, ident: Ident, rhs: AssignRHS, symbTable: SymbolTable): Unit = {
    if (symbTable.containScope(ident)) {
      semanticErrors += DeclaredVarErr(ident)
      return
    }
    
    val rType = rhs.getType(symbTable)
    if (rhs.semanticErrors.nonEmpty) {
      semanticErrors ++= rhs.semanticErrors
      symbTable.add(ident, tpe)
      return
    }

    symbTable.add(ident, tpe)
    if (tpe != rType) {
      semanticErrors += MismatchTypesErr(rhs, rType, List(tpe))
    }
  }


  private def checkOtherRead(lhs : AssignRHS, symbTable : SymbolTable) : Unit = {
		val otherType = checkType(lhs, symbTable)
		otherType match {
			case Ast.CharType | Ast.Int =>
			case _ => semanticErrors += MismatchTypesErr(lhs, otherType, List(CharType, Int))				
		}
	}

  private def checkAssign(lhs: AssignLHS, rhs: AssignRHS, symbTable: SymbolTable): Unit
    = lhs match {
      case ident: Ident     =>
        checkEqAssignType(ident, rhs, symbTable)
      case aElem: ArrayElem =>
        checkAssignType(checkType(aElem, symbTable), rhs, symbTable)
      case pElem: PairElem  =>
        checkAssignType(checkType(pElem, symbTable), rhs, symbTable)
  }

  
  private def checkEqAssignType(ident: Ident, rhs: AssignRHS, symbTable: SymbolTable): Unit = {
    if (!symbTable.contains(ident)) {
      semanticErrors += NotDeclaredVarErr(ident)
      return
    }

    if (symbTable.isFunc(ident)) {
      semanticErrors += IllegalAssignFuncErr(ident)
      return
    }

    checkAssignType(checkType(ident, symbTable), rhs, symbTable)
  }

  private def checkAssignType(lType: Type, rhs: AssignRHS, symbTable: SymbolTable) = {
    val rType = checkType(rhs, symbTable)
    
    if ((lType != rType) && (lType != null) && (rType != null)) {
      semanticErrors += MismatchTypesErr(rhs, rType, List(lType))
    }
  }

	private def checkReturn(expr: Expr, symbTable: SymbolTable): Unit = {
		val expected = symbTable.getFuncReturnType

		if(expected == null){
			semanticErrors += InvalidRetErr(expr)
			return        
		}

		val tpe = checkType(expr, symbTable)

		if(expected != tpe){
			semanticErrors += MismatchTypesErr(expr, tpe, List(expected))
		}
	}

	private def checkExit(expr: Expr, symbTable: SymbolTable) = {
		val tpe = checkType(expr, symbTable)

		if((tpe != null) && (tpe != Int)){
			semanticErrors += MismatchTypesErr(expr, tpe, List(Int))
		}
	}

	private def checkIf(cond : Expr, statThen: Stat, statElse: Stat, symbTable : SymbolTable) = {
		val tpe = checkType(cond, symbTable)
		if((tpe != null) && (tpe != Bool)){
				semanticErrors += MismatchTypesErr(cond, tpe, List(Bool))
		}
		
		checkStat(statThen, symbTable.nextScope)
		checkStat(statElse, symbTable.nextScope)
	}

	private def checkWhile(cond: Expr, stat: Stat, symbTable: SymbolTable) = {
		val tpe = checkType(cond, symbTable)
		if((tpe != null) && (tpe != Bool)){
				semanticErrors += MismatchTypesErr(cond, tpe, List(Bool))
		}

		checkStat(stat, symbTable)
	}

	private def checkType(assignRHS : AssignRHS, symbTable : SymbolTable) : Type = {
		val tpe = assignRHS.getType(symbTable)
		
		if(assignRHS.semanticErrors.nonEmpty){
			semanticErrors ++= assignRHS.semanticErrors
			return null
		}
    
    return tpe
	}

	private def checkFree(expr : Expr, symbolTable : SymbolTable): Unit = {
		val tpe = checkType(expr, symbolTable)
		if ((tpe == null) || tpe.isPair || tpe.isArray) {
			return
		}
    semanticErrors += MismatchTypesErr(expr, tpe, List(Pair(null, null), ArrayType(null)))
	}

	// private def sameBaseType(tpeOne : BaseType, tpeTwo : BaseType) : Boolean = {
	// 	tpeOne match {
	// 		case String     => tpeTwo match {
	// 			case String   => true
	// 			case _        => false		
	// 		}
	// 		case Int        => tpeTwo match {
	// 			case Int      => true
	// 			case _        => false
	// 		}
	// 		case CharType   => tpeTwo match {
	// 			case CharType => false
	// 			case _        => false
	// 		}
	// 		case Bool       => tpeTwo match {
	// 			case Bool     => false 
	// 			case _        => false
	// 	  }
  //   }
  // }

  private def checkRead(assignL : AssignLHS, symbTable : SymbolTable) : Unit = {
		assignL match {
      case ident: Ident     => readStat(ident, symbTable)
      case array: ArrayElem => readStat(array, symbTable)
      case pair: PairElem   => readStat(pair, symbTable)
    }
  }

  private def readStat(assignR: AssignRHS, symbTable: SymbolTable) : Unit = {
    val rType = checkType(assignR, symbTable)
    rType match {
      case CharType | Int => 
      case _              => 
        semanticErrors += MismatchTypesErr(assignR, rType, List(CharType, Int))
    }
  }

}
