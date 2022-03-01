package backend.DefinedFuncs

import backend.CodeGen.{dataTable, funcTable, resultRegister}


import backend.Operand._
import backend.DefinedFuncs.PrintInstrs._
import backend.DefinedFuncs.RuntimeErrors._
import scala.collection.mutable.ListBuffer
import backend.Condition._
import backend.Opcodes._

object PreDefinedFuncs {
  sealed trait PreDefFunc {
    val functionLabel: Label
    val msgName: List[String]
    val msgs: List[String]
    val function: (Label, ListBuffer[Instr])
  }

  case object ArrayBounds extends PreDefFunc {
    override val functionLabel = Label("p_check_array_bounds")
    override val msgName = List("msg_neg_index", "msg_index_too_large")
    override val msgs = List(
      "ArrayIndexOutOfBoundsError: negative index\\n\\0",
      "ArrayIndexOutOfBoundsError: index too large\\n\\0"
    )
    override val function = checkArrayBounds
  }
  case object DivideByZero extends PreDefFunc {
    override val functionLabel = Label("p_check_division_by_zero")
    override val msgName = List("msg_division_by_zero")
    override val msgs =
      List("DivisionByZeroError: division or modulo by zero\\n\\0")
    override val function = checkDivideByZero
  }

  case object Overflow extends PreDefFunc {
    override val functionLabel = Label("p_throw_overflow_error")
    override val msgName = List("msg_overflow")
    override val msgs =
      List(
        "OverflowError: the result is too large/small to be stored as a 4-byte signed-integer.\\n"
      )
    override val function = throwOverflowError
  }

  case object FreePair extends PreDefFunc {
    override val functionLabel = Label("p_free_pair")
    override val msgName = List("msg_null_reference")
    override val msgs =
      List("NullReferenceError: dereferencing a null reference\\n\\0")
    override val function = freePair
  }

  case object FreeArray extends PreDefFunc {
    override val functionLabel = Label("p_free_array")
    override val msgName = List("msg_null_reference")
    override val msgs =
      List("NullReferenceError: dereferencing a null reference\\n\\0")
    override val function = freeArray
  }

  case object NPE extends PreDefFunc {
    override val functionLabel = Label("p_check_null_pointer")
    override val msgName = List("msg_null_reference")
    override val msgs =
      List("NullReferenceError: dereferencing a null reference\\n\\0")
    override val function = checkNullPointer
  }

  case object RuntimeError extends PreDefFunc {
    override val functionLabel = Label("p_throw_runtime_error")
    override val msgName = List.empty[String]
    override val msgs = List.empty[String]
    override val function = throwRuntimeError
  }

  case object PutChar extends PreDefFunc {
    override val functionLabel = Label("putchar")

    override val msgName = List.empty[String]

    override val msgs = List.empty[String]

    override val function = null
  }


  case object PrintInt extends PreDefFunc {
    override val functionLabel = Label("p_print_int")
    override val msgName = List("msg_int")
    override val msgs = List("%d\\0")
    override val function = intPrintInstrs
  }

  case object PrintBool extends PreDefFunc {
    override val functionLabel = Label("p_print_bool")
    override val msgName = List("msg_true", "msg_false")
    override val msgs = List("true\\0", "false\\0")
    override val function = boolPrintInstrs
  }

  case object PrintString extends PreDefFunc {
    override val functionLabel = Label("p_print_string")
    override val msgName = List("msg_string")
    override val msgs = List("%.*s\\0")
    override val function = stringPrintInstrs
  }

  case object PrintReference extends PreDefFunc {
    override val functionLabel = Label("p_print_reference")
    override val msgName = List("msg_reference")
    override val msgs = List("%p\\0")
    override val function = referencePrintInstrs
  }

  case object PrintLn extends PreDefFunc {
    override val functionLabel = Label("p_print_ln")
    override val msgName = List("msg_new_line")
    override val msgs = List("\\0")
    override val function = newLinePrintInstrs
  }



  case object ReadInt extends PreDefFunc {
    override val functionLabel = Label("p_read_int")

    override val function = null
    override val msgs = List("%d\\0")
    override val msgName = List.empty
  }

  case object ReadChar extends PreDefFunc {
    override val functionLabel = Label("p_read_char")

    override val function = null
    override val msgs = List(" %c\\0")
    override val msgName = List.empty
  }

}
