package frontend
import Ast._

sealed trait SemanticError {
  override def toString: String = this match {
    case MismatchTypesErr(invalid, actualT, expected) =>
      "Type mismatch at " + invalid + " expected type " + expected.mkString( " or "
      ) + " but actual type " + actualT
    case OutOfBoundErr(id) => "Array index out of bounds for array " + id
    case AccessDeniedErr(id) => "Element access of " + id + " is not permitted"
    case InvalidRetErr(e) => "Invalid return statement from main: return " + e
    case IllegallAssignFuncErr(id) => "Illegal assignment to function " + id
    case InvalidParamsErr(id, actual, expected) =>
      "Invalid params for function " + id + " Expected number of params: " + expected + ". Actual: " + actual
    case InvalidPairElemErr(pe) =>
      "Invalid input, expected: Pair, unable to perform " + pe
    case DeclaredFuncErr(id)    => DeclaredIdentErr(id).toString()
    case NotDeclaredFuncErr(id) => NotDeclaredIdenErr("Function", id).toString
    case NotDeclaredVarErr(id) => NotDeclaredIdenErr("Variable", id).toString()
    case DeclaredVarErr(id)    => DeclaredIdentErr(id).toString()
    case NotDeclaredIdenErr(t, id) =>
      t + " " + id + " not declared in current scope"
    case DeclaredIdentErr(id) => "Conflicting definitions for variable " + id
    case IllegalFree(expr) => expr.toString + " is not available to free."
  }
}

case class MismatchTypesErr(invalid: AssignRHS, actualT: Type, expected: List[Type])
    extends SemanticError
case class NotDeclaredVarErr(id: Ident) extends SemanticError
case class DeclaredVarErr(id: Ident) extends SemanticError
case class OutOfBoundErr(id: Ident) extends SemanticError
case class AccessDeniedErr(id: Ident) extends SemanticError
case class InvalidRetErr(e: Expr) extends SemanticError
case class IllegallAssignFuncErr(id: Ident) extends SemanticError
case class DeclaredFuncErr(id: Ident) extends SemanticError
case class NotDeclaredFuncErr(id: Ident) extends SemanticError
case class InvalidParamsErr(id: Ident, actual: Int, expected: Int)
    extends SemanticError
case class InvalidPairElemErr(pe: PairElem) extends SemanticError
case class DeclaredIdentErr(id: Ident) extends SemanticError
case class NotDeclaredIdenErr(t: String, id: Ident) extends SemanticError
case class IllegalFree(expr: Expr) extends SemanticError