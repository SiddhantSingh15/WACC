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
  
  def funcParamMatch(id: Ident, args: Option[ArgList]): List[SemanticError] = {
    val meta = funcMap.get(id)
    if (meta.isEmpty) {
      return List[SemanticError](functionNotDeclared(id: Ident))
    }
    val Some(Meta(_, value)) = meta
    if (args.isEmpty) {
      if (value.exists(_.isEmpty)) {
        return List[SemanticError]()
      }
      return List(invalidParams(id, 0, value.get.length))
    }
    val argList = args.get
    val pList = value.get
    val paramLen = pList.length
    val argLen = argList.length
    if (argLen != paramLen) {
      return List(invalidParams(id, argLen, paramLen))
    }
    var result = List[SemanticError]()
    for (i <- 0 until argLen) {
      val argType = argList.lift(i).getType(this)
      val paramType = pList(i)
      if (argType != paramType) {
        result ::= typeMismatch(argList(i), argType, List(paramType))
      }
    }
    result
  }
}