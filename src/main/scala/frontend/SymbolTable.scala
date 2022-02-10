package frontend

import scala.collection.mutable.HashMap
import scala.collection.mutable
import Ast._

case class Meta(t: Type, pList: Option[List[Type]])

case class SymbolTable(
    prev: SymbolTable,
    funcId: Ident,
    funcMap: HashMap[Ident, Meta]) 
  {

  val varMap = new HashMap[Ident, Type]

  def nextScope(nextFunc: Ident): SymbolTable = {
    SymbolTable(this, nextFunc, funcMap)
  }

  def nextScope: SymbolTable = {
    SymbolTable(this, funcId, funcMap)
  }

  def lookupAll(id: Ident): Type = {
    var curSymbol = this
    while (curSymbol != null) {
      val d = curSymbol.varMap
      if (d.contains(id)) {
        return d.apply(id)
      }
      curSymbol = curSymbol.prev
    }
    val t = funcMap.get(id)
    if (t.isEmpty) {
      return null
    }
    t.get.t
  }

  def add(id: Ident, t: Type): Unit = {
    varMap.addOne(id, t)
  }

  def contains(id: Ident): Boolean = {
    lookupAll(id) != null
  }

  def containScope(id: Ident): Boolean = {
    varMap.contains(id)
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

  def addFunctions(funcs: List[(Ident, Meta)]): mutable.ListBuffer[SemanticError] = {
    var semErrors = mutable.ListBuffer.empty[SemanticError]
    for (f <- funcs) {
      if (funcMap.contains(f._1)) {
        semErrors += DeclaredFuncErr(f._1)
      } else {
        funcMap.addOne(f)
      }
    }
    semErrors
  }

  def getFuncRetType: Type = {
    if (funcId == null) {
      return null
    }
    val funcRet = funcMap.get(funcId)
    if (funcRet.isEmpty) {
      return null
    }
    funcRet.get.t
  }

  def isFunc(id: Ident): Boolean = {
    val meta = funcMap.get(id)
    meta.isDefined
  }
  
  def parameterMatch(id: Ident, args: Option[ArgList]): List[SemanticError] = {
    val meta = funcMap.get(id)
    if (meta.isEmpty) {
      return List[SemanticError](NotDeclaredFuncErr(id: Ident))
    }
    val Some(Meta(_, value)) = meta
    if (args.isEmpty) {
      if (value.exists(_.isEmpty)) {
        return List[SemanticError]()
      }
      return List(InvalidParamsErr(id, 0, value.get.length))
    }
    val argList = args.get.exprs
    val pList = value.get
    val paramLen = pList.length
    val argLen = argList.length
    if (argLen != paramLen) {
      return List(InvalidParamsErr(id, argLen, paramLen))
    }
    var result = List[SemanticError]()
    for (i <- 0 until argLen) {
      val argType = argList(i).getType(this)
      val paramType = pList(i)
      if (argType != paramType) {
        return result ++ List(MismatchTypesErr(argList(i), argType, List(paramType)))
      }
    }
    result
  }
}