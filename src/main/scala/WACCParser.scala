import parsley.Parsley, Parsley._
import scala.language.implicitConversions
import ast._

object parser {
    import parsley.combinator.{between, eof, option, sepBy1, many}
    import parsley.lift.{lift2, lift4}

    val param: Parsley[Param] = lift2(Param, types, identifier) ? "param"

    val paramList: Parsley[ParamList] = ParamList <#> sepBy1(param, ",") ? "param-list"

    private def statTerminates(stat: Stat): Boolean = stat match {
    case If(_, s1, s2)       => statTerminates(s1) && statTerminates(s2)
    case While(_, s)         => statTerminates(s)
    case Begin(s)            => statTerminates(s)
    case Seq(s)              => statTerminates(s.last)
    case Exit(_) | Return(_) => true
    case _                   => false
  }
}

