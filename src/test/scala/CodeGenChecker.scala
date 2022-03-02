import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.Paths
import java.io.File
import parsley.Success
import parsley.Failure
import frontend._
import backend._

class CodeGenChecker extends AnyFunSuite {

    val parser = frontend.Parser;
    val codeGen = backend.CodeGen;
    val prettyPrinter = backend.PrettyPrinter;
    val file = new File("wacc_examples/valid/basic/exit/exitBasic.wacc")
    val programTree = parser.parseFromFile(file).get.get
    val (data, instructions) = codeGen.transProgram(programTree)
    prettyPrinter.prettyPrint(file.getName(), data, instructions)

    assert(true)
}

