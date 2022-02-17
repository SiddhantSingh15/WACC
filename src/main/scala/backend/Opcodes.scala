package backend

object Opcodes {
    sealed trait Opcode 

    // ARITHMETIC

    case class Add() extends Opcode
    case class Sub() extends Opcode
    case class Mul() extends Opcode

    // COMPARE

    case class Cmp() extends Opcode

    // BRANCH

    case class B() extends Opcode
    case class Bl() extends Opcode
    
}
