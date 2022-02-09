package compiler
import scala.collection.mutable.HashMap
import Ast._

case class Meta(t: Type, pList: Option[List[Type]])

case class SymbolTable(
    parent: SymbolTable,
    funcId: Ident,
    funcMap: HashMap[Ident, Meta]
) {

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
      curSymbol = curSymbol.parent
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

  def addVariables(vars: List[(Ident, Type)]): List[SemanticError] = {
    var semErrors: List[SemanticError] = List.empty[SemanticError]
    for (v <- vars) {
      if (varMap.contains(v._1)) {
        semErrors ::= variableDeclared(v._1)
      } else {
        varMap.addOne(v)
      }
    }
    semErrors
  }

  def addFunctions(funcs: List[(Ident, Meta)]): List[SemanticError] = {
    var semErrors: List[SemanticError] = List.empty[SemanticError]
    for (f <- funcs) {
      if (funcMap.contains(f._1)) {
        semErrors ::= functionDeclared(f._1)
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
  // TODO : implement parameter matching
  // check if param supplied > arg of function
  // check param type is the same as arg type
}