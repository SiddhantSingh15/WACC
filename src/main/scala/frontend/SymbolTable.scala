package frontend

import scala.collection.mutable.HashMap
import scala.collection.mutable
import backend.CodeGen._
import AST._

case class Info(t: Type, pList: Option[List[Type]])

case class SymbolTable(
    prev: SymbolTable,
    funcId: Ident,
    funcMap: HashMap[Ident, Info]) 
  {

  val children = mutable.ListBuffer.empty[SymbolTable]

  val varMap = new HashMap[Ident, (Int, Type)]

  def nextScope(nextFunc: Ident): SymbolTable = {
    val next = SymbolTable(this, nextFunc, funcMap)
    children += next
    next
  }

  def nextScope: SymbolTable = {
    val next = SymbolTable(this, funcId, funcMap)
    children += next
    next
  }

  def getNextScope: SymbolTable = {
    val next = children.head
    children -= next
    next
  }

  def lookupAll(ident: Ident): Type = {
    var current = this
    while (current != null) {
      var vMap = current.varMap
      if (vMap.contains(ident)) {
        return vMap.apply(ident)._2
      }
      current = current.prev
    }
    val t = funcMap.get(ident)
    if (t.isEmpty) {
      return null
    }
    t.get.t
  }

  def add(ident: Ident, n: Int, t: Type): Unit = {
    varMap.addOne(ident, (n, t))
  }

  def add(ident: Ident, t: Type): Unit = {
    varMap.addOne(ident, (0, t))
  }

  def lookupCG(ident: Ident): (Int, Type) = {
    var currentST = this
    while (currentST != null) {
      val vMap = currentST.varMap
      if (vMap.contains(ident)) {
        val (i, t) = vMap.apply(ident)
        if (i != 0) {
          return (i, t)
        }
      }
      currentST = currentST.prev
    }
    null
  }

  def contains(ident: Ident): Boolean = {
    lookupAll(ident) != null
  }

  def containScope(ident: Ident): Boolean = {
    varMap.contains(ident)
  }

  def addVariables(vars: List[(Ident, Type)]): 
                   mutable.ListBuffer[SemanticError] = {
    var semanticErrors = mutable.ListBuffer.empty[SemanticError]
    for (v <- vars) {
      if (varMap.contains(v._1)) {
        semanticErrors += DeclaredVarErr(v._1)
      } else {
        add(v._1, v._2)
      }
    }
    semanticErrors
  }

  def addFunctions(funcs: List[(Ident, Info)]): 
                   mutable.ListBuffer[SemanticError] = {
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

  def apply(expr: Ident): (Int, Type) = {
    lookupCG(expr)
  }
  
  def parameterMatch(ident: Ident, args: Option[ArgList]): 
                    mutable.ListBuffer[SemanticError] = {
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

  def spMaxDepth: Int = {
    varMap.toList.map(x => getTypeSize(x._2._2)).sum
  }

  def spMaxDepth(id: Ident): Int = {
    val Info(_, pList) = funcMap(id)
    pList match {
      case Some(p) =>
        spMaxDepth - p.map(x => getTypeSize(x)).sum
      case None => spMaxDepth
    }
  }

}