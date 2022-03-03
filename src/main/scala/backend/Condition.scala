package backend

object Condition {

  sealed trait Condition {
    val oppositeCmp: Condition = null
  }
  //Equal
  case object EQ extends Condition {
    override val oppositeCmp: Condition = NE
  }
  //Not Equal
  case object NE extends Condition {
    override val oppositeCmp: Condition = EQ
  }
  // Less than
  case object LT extends Condition {
    override val oppositeCmp: Condition = GE
  }
  // Less than Equal
  case object LE extends Condition {
    override val oppositeCmp: Condition = GT
  }
  //Greater Than
  case object GT extends Condition {
    override val oppositeCmp: Condition = LE
  }
  // Greater than Equal
  case object GE extends Condition {
    override val oppositeCmp: Condition = LT
  }
  // Overflow
  case object OF extends Condition {
    override def toString: String = "VS"
  }
  // Carry Set
  case object CS extends Condition

}
