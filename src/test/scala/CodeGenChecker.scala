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
    val semChecker = frontend.SemanticChecker
    val file = new File("wacc_examples/valid/runtimeErr/divideByZero/divZero.wacc")
    val programTree = parser.parseFromFile(file).get.get
    val (symbTable, semRes) = semChecker.checkProgram(programTree)
    val (data, instructions) = codeGen.transProgram(programTree, symbTable)
    prettyPrinter.prettyPrint(file.getName(), data, InstrEval.optimiseBlocks(instructions))

    assert(true)
}

