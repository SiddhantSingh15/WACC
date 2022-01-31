class CompilerTest extends org.scalatest.funsuite.AnyFunSuite {
  test("Compiler.cube") {
    assert(Compiler.cube(3) === 27)
  }
}
