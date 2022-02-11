package frontend

import scala.collection.mutable.HashMap
import scala.collection.mutable
import Ast._

case class Info(t: Type, pList: Option[List[Type]])

case class SymbolTable(
    prev: SymbolTable,
    funcId: Ident,
    funcMap: HashMap[Ident, Info]) 
  {

  val varMap = new HashMap[Ident, Type]

  def nextScope(nextFunc: Ident): SymbolTable = {
    SymbolTable(this, nextFunc, funcMap)
  }

  def nextScope: SymbolTable = {
    SymbolTable(this, funcId, funcMap)
  }

  def lookupAll(ident: Ident): Type = {
    var current = this
    while (current != null) {
      var vMap = current.varMap
      if (vMap.contains(ident)) {
        return vMap.apply(ident)
      }
      current = current.prev
    }
    val t = funcMap.get(ident)
    if (t.isEmpty) {
      return null
    }
    t.get.t
  }

  def add(ident: Ident, t: Type): Unit = {
    varMap.addOne(ident, t)
  }

  def contains(ident: Ident): Boolean = {
    lookupAll(ident) != null
  }

  def containScope(ident: Ident): Boolean = {
    varMap.contains(ident)
  }

  def addVariables(vars: List[(Ident, Type)]): mutable.ListBuffer[SemanticError] = {
    var semanticErrors = mutable.ListBuffer.empty[SemanticError]
    for (v <- vars) {
      if (varMap.contains(v._1)) {
        semanticErrors += DeclaredVarErr(v._1)
      } else {
        varMap.addOne(v)
      }
    }
    semanticErrors
  }

  def addFunctions(funcs: List[(Ident, Info)]): mutable.ListBuffer[SemanticError] = {
    var semanticErrors = mutable.ListBuffer.empty[SemanticError]
    for (f <- funcs) {
      if (funcMap.contains(f._1)) {
        semanticErrors += DeclaredFuncErr(f._1)
      } else {
        funcMap.addOne(f)
      }
    }
    semanticErrors
  }

  def getFuncReturnType: Type = {
    if (funcId == null) {
      return null
    }
    val funcRet = funcMap.get(funcId)
    if (funcRet.isEmpty) {
      return null
    }
    funcRet.get.t
  }

  def isFunc(ident: Ident): Boolean = {
    val info = funcMap.get(ident)
    info.isDefined
  }
  
  def parameterMatch(ident: Ident, args: Option[ArgList]): mutable.ListBuffer[SemanticError] = {
    val info = funcMap.get(ident)
    if (info.isEmpty) {
      return mutable.ListBuffer[SemanticError](NotDeclaredFuncErr(ident: Ident))
    }

    val Some(Info(_, value)) = info
    if (args.isEmpty) {
      if (value.exists(_.isEmpty)) {
        return mutable.ListBuffer[SemanticError]()
      }
      return mutable.ListBuffer(InvalidParamsErr(ident, 0, value.get.length))
    }

    val argList = args.get.exprs
    val pList = value.get
    val paramLen = pList.length
    val argLen = argList.length

    if (argLen != paramLen) {
      return mutable.ListBuffer(InvalidParamsErr(ident, argLen, paramLen))
    }

    var result = mutable.ListBuffer.empty[SemanticError]
    for (i <- 0 until argLen) {
      val argType = argList(i).getType(this)
      val paramType = pList(i)
      if (argType != paramType) {
        result += MismatchTypesErr(argList(i), argType, List(paramType))
      }
    }
    result
  }
  
}