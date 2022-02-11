package frontend
import Ast._

sealed trait SemanticError {
  override def toString: String = this match {
    case MismatchTypesErr(invalid, actualT, expected) =>
      "Type mismatch at " + invalid + " expected type " + expected.mkString( " or "
      ) + " but actual type " + actualT
    case OutOfBoundErr(ident) => "Array index out of bounds for array " + ident
    case AccessDeniedErr(ident) => "Element access of " + ident + " is not permitted"
    case InvalidRetErr(e) => "Invalid return statement from main: return " + e
    case IllegalAssignFuncErr(ident) => "Illegal assignment to function " + ident
    case InvalidParamsErr(ident, actual, expected) =>
      "Invalid params for function " + ident + " Expected number of params: " + expected + ". Actual: " + actual
    case InvalidPairElemErr(pe) =>
      "Invalid input, expected: Pair, unable to perform " + pe
    case DeclaredFuncErr(ident)    => DeclaredIdentErr(ident).toString()
    case NotDeclaredFuncErr(ident) => NotDeclaredIdenErr("Function", ident).toString
    case NotDeclaredVarErr(ident) => NotDeclaredIdenErr("Variable", ident).toString()
    case DeclaredVarErr(ident)    => DeclaredIdentErr(ident).toString()
    case NotDeclaredIdenErr(t, ident) =>
      t + " " + ident + " not declared in current scope"
    case DeclaredIdentErr(ident) => "Conflicting definitions for variable " + ident + "."
    case IllegalFree(expr) => expr.toString + " is not available to free."
    case FuncNoRetErr(ident) => "Function \'" + ident.toString + "\' does not exit with " +
      "{return, if, while, exit}" +
      " or it is mutually recursive with another function."
  }
}

case class MismatchTypesErr(invalid: AssignRHS, actualT: Type, expected: List[Type])
    extends SemanticError
case class NotDeclaredVarErr(ident: Ident) extends SemanticError
case class DeclaredVarErr(ident: Ident) extends SemanticError
case class OutOfBoundErr(ident: Ident) extends SemanticError
case class AccessDeniedErr(ident: Ident) extends SemanticError
case class InvalidRetErr(e: Expr) extends SemanticError
case class IllegalAssignFuncErr(ident: Ident) extends SemanticError
case class DeclaredFuncErr(ident: Ident) extends SemanticError
case class NotDeclaredFuncErr(ident: Ident) extends SemanticError
case class InvalidParamsErr(ident: Ident, actual: Int, expected: Int)
    extends SemanticError
case class InvalidPairElemErr(pe: PairElem) extends SemanticError
case class DeclaredIdentErr(ident: Ident) extends SemanticError
case class NotDeclaredIdenErr(t: String, ident: Ident) extends SemanticError
case class IllegalFree(expr: Expr) extends SemanticError
case class FuncNoRetErr(ident: Ident) extends SemanticError