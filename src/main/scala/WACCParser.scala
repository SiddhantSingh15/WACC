import parsley.Parsley, Parsley._
import scala.language.implicitConversions

object parser {
    import parsley.combinator.{between, eof, option, sepBy1, many}
    import parsley.lift.{lift2, lift4}
    import WACCLexer._ 

    val param: Parsley[Param] = lift2(Param, types, identifier) ? "param"

    val paramList: Parsley[ParamList] = ParamList <#> sepBy1(param, ",") ? "param-list"
}

