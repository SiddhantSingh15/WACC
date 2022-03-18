package frontend

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import backend.CodeGeneration.CodeGenHelper._
import AST._
import backend.CodeGen._

case class Info(t: Type, pList: Option[List[Type]])

case class SymbolTable(prev: SymbolTable, funcId: Ident,
  funcMap: HashMap[Ident, Info]) {

  val children = ListBuffer.empty[SymbolTable]

  val varMap = new HashMap[Ident, (Int, Type, Option[AssignRHS])]

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

  def add(ident: Ident, n: Int, t: Type, value: Option[AssignRHS]): Unit = {
    value match {
      case Some(_: Heap) =>
        varMap.addOne(ident, (n, t, None))
      case Some(Call(_, _)) =>
        varMap.addOne(ident, (n, t, None))
      case _ =>
        varMap.addOne(ident, (n, t, value))  
    }
  }

  def add(ident: Ident, t: Type): Unit = {
    varMap.addOne(ident, (0, t, None))
  }

  def updatePair(id: Ident, pos: Int, rhs: AssignRHS): Unit = {
    if (!rhs.isInstanceOf[Expr]) return
    val value = this.getValue(id).get
    val (_, tpe) = this(id)
    if (!value.isInstanceOf[NewPair]) return
    val NewPair(expr1, expr2) = value
    var maybeValue = rhs
    rhs match {
      case ident: Ident =>
        if (this.getValue(ident).isEmpty) {
          maybeValue = null
        }
      case _ => 
    }
    pos match {
      case 1 => 
        updateValue(id, tpe, 
          Some(NewPair(maybeValue.asInstanceOf[Expr], expr2)))
      case 2 => 
        updateValue(id, tpe, 
          Some(NewPair(expr1, maybeValue.asInstanceOf[Expr])))
      case _ =>
    }
  }

  def updateArray(id: Ident, exprList: List[Expr], rhs: AssignRHS): Unit = {
    var ident = id

    val (_, tpe) = symbTable(id)

    var i = 0
    var found = false
    while (!found) {
      var value = reduceRHS(ident)
      if (!value.isInstanceOf[ArrayLiter]) return
      var ArrayLiter(exprs) = value
      var IntLiter(index) = reduceRHS(exprList(i))
      if (index < 0 || index >= exprs.size) return
      var maybeValue = exprs(index)

      maybeValue match {
        case Ident(_) =>
          ident = maybeValue.asInstanceOf[Ident]
          i += 1
        case _ =>
          found = true
          val (_, tpe) = symbTable(ident)
          updateValue(ident, tpe, 
            Some(ArrayLiter(exprs.updated(index, rhs.asInstanceOf[Expr]))
            .asInstanceOf[AssignRHS]))
      }
    }
  }

  def updateValue(ident: Ident, t: Type, value: Option[AssignRHS]): Unit = {
    var maybeValue = value
    (inBeginEndScope, value) match {
      case (false, _) =>
        maybeValue = None
      case (_, Some(Call(_, _))) =>
        maybeValue = None
      case _ => 
    }

    var currentST = this
    
    while (currentST != null) {
      val vMap = currentST.varMap
      if (vMap.contains(ident)) {
        val (i, tpe, v) = vMap(ident)
        if (tpe == t) {
            vMap(ident) = (i, t, maybeValue)
          }
      }
      currentST = currentST.prev
    }
  }


  def lookupCG(ident: Ident): (Int, Type, Option[AssignRHS]) = {
    var currentST = this
    while (currentST != null) {
      val vMap = currentST.varMap
      if (vMap.contains(ident)) {
        val (i, t, v) = vMap.apply(ident)
        if (i != 0) {
          return (i, t, v)
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
                   ListBuffer[SemanticError] = {
    var semanticErrors = ListBuffer.empty[SemanticError]
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
                   ListBuffer[SemanticError] = {
    var semanticErrors = ListBuffer.empty[SemanticError]
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

  def apply(id: Ident): (Int, Type) = {
    val (i, t, v) = lookupCG(id)
    (i, t)
  }

  def getValue(id: Ident): Option[AssignRHS] = {
    if (varMap.contains(id)) {
      varMap(id)._3
    } else {
      None
    }
  }

  
  def parameterMatch(ident: Ident, args: Option[ArgList]): 
                    ListBuffer[SemanticError] = {
    val info = funcMap.get(ident)
    if (info.isEmpty) {
      return ListBuffer[SemanticError](NotDeclaredFuncErr(ident: Ident))
    }

    val Some(Info(_, value)) = info
    if (args.isEmpty) {
      if (value.exists(_.isEmpty)) {
        return ListBuffer[SemanticError]()
      }
      return ListBuffer(InvalidParamsErr(ident, 0, value.get.length))
    }

    val argList = args.get.exprs
    val pList = value.get
    val paramLen = pList.length
    val argLen = argList.length

    if (argLen != paramLen) {
      return ListBuffer(InvalidParamsErr(ident, argLen, paramLen))
    }

    var result = ListBuffer.empty[SemanticError]
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