import parsley.Parsley._
import parsley.implicits.character.{charLift, stringLift}
import parsley.{Parsley, Result}
import parsley.character.{anyChar, char, digit, letter, noneOf, upper}
import parsley.combinator.{many, option}
import parsley.lift.lift2
import Ast._
import parsley.expr.{GOps, Levels, Ops, Postfix, Prefix, chain, precedence}

object parser {
  lazy val baseType: Parsley[BaseType] = 
    ("int" #> Int) <|> ("bool" #> Bool) <|> ("char" #> Char) <|> ("string" #> String)

  val unaryOp: Parsley[UnOp] =
    ('!' #> Not) <|> ('-' #> Negation) <|> ("len" #> Len) <|> ("ord" #> Ord) <|> ("chr" #> Chr)

  val binaryOp: Parsley[BinOp] =
    ('*' #> Mul) <|> ('/' #> Div) <|> ('%' #> Mod) <|> ('+' #> Plus) <|>
      ('-' #> Sub) <|> ('>' #> GT) <|> (">=" #> GTE) <|> ('<' #> LT) <|>
      ("<=" #> LTE) <|> ("==" #> Equal) <|> ("!=" #> NotEqual) <|> ("&&" #> And) <|> ("||" #> Or)

  val natural: Parsley[Int] =
    digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit)

  val intSign: Parsley[IntSign] = ('+' #> Pos) <|> ('-' #> Neg)
}

