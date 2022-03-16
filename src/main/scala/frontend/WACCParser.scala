package frontend

import parsley.Parsley, Parsley._
import scala.language.implicitConversions
import AST._
import lexer._
import parsley.character.{noneOf, oneOf}
import SyntaxErrors.{TestError, TestErrorBuilder}
import parsley.combinator.{many, sepBy, some, sepEndBy, sepBy1}
import parsley.lift.{lift1, lift2, lift3, lift4}
import parsley.expr.{Atoms, InfixL, InfixR, NonAssoc, Ops, Prefix, SOps, Postfix, chain, precedence}
import parsley.implicits.character.{charLift}
import implicits.implicitToken
import parsley.errors.ErrorBuilder
import parsley.errors.combinator._
import parsley.Result
import parsley.io.ParseFromIO
import java.io.File
import scala.util.Try
import scala.io.Codec

object Parser {
    
  val eb = new TestErrorBuilder

  def parse[Err: ErrorBuilder](input: String): Result[TestError, WaccProgram] = `<program>`.parse[TestError](input)(eb)
  def parseFromFile[Err: ErrorBuilder](file: File): Try[Result[TestError, WaccProgram]] = `<program>`.parseFromFile[TestError](file)(eb, Codec.ISO8859)
  
  private lazy val `<base-type>`: Parsley[BaseType] = 
    ("int" #> Int) <|> 
    ("bool" #> Bool) <|> 
    ("char" #> CharType) <|> 
    ("string" #> String)  
  
  private lazy val `<type>` : Parsley[Type] =
    attempt(`<array-type>`) <|> attempt(`<pointer-type>`) <|> `<base-type>` <|> `<pair-type>`
  
  private lazy val `<pointer-type>` : Parsley[PointerType] =
    chain.postfix1((`<base-type>` <|> `<pair-type>`), "~" #> PointerType)

  private lazy val `<array-type>` : Parsley[ArrayType] =
    chain.postfix1((`<base-type>` <|> `<pair-type>`), "[]".label("to be part of an ArrayType") #> ArrayType)

  private lazy val `<pair-elem-type>`: Parsley[PairElemType] =
    "pair" #> PairElemPair <|> (PairElemWithType <#> `<type>`)

  private lazy val `<pair-type>` : Parsley[PairType] =
    ("pair" *> parens(
        lift2(Pair, `<pair-elem-type>`,"," *> `<pair-elem-type>`))
    )

  private val intSign = ("+" #> identity[Long] _ <|> "-" #> ((x: Long) => -x))
  
  private val `<int-liter>` : Parsley[IntLiter] = 
    amend { lift1(IntLiter, (intSign <*> NUMBER.label("number after integer sign") <|> NUMBER)
    .collectMsg("Integer Overflow") 
    {case x if x.toInt == x => x.toInt}) }
  
  private val `<char-liter>` : Parsley[CharLiter] = 
    (CharLiter <#>  '\'' *> `<character>` <* '\'')

  private val `<str-liter>` : Parsley[StrLiter] =
    StrLiter <#> ('\"' *> (many(`<character>`)) <* '\"'.label("end of string")).label("string")     
  
  private val `<escaped-char>` : Parsley[Char] = 
    oneOf(
        '0','b','t','n','f','r','"','\'','\\'
    )
    .label("end of escape sequence")
    .explain("valid escape sequences include \\0, \\b, \\t, \\n, \\f, \\r, \\\", \\' or \\\\")

  
  private val `<character>` : Parsley[Character] =
    amend {EscapeCharacter <#> ("\\" *> `<escaped-char>`) }.label("escape character") <|>
    (NormalCharacter <#> noneOf('\\', '\'', '"')).label("string character")    
  
  private val `<bool-liter>`: Parsley[BoolLiter] = 
    ("true" #> True) <|> ("false" #> False)  
  
  private val `<pair-elem>` : Parsley[PairElem] = 
    (Fst <#> ("fst" *> `<expr>`)) <|>
    (Snd <#> ("snd" *> `<expr>`))
  
  private val `<assign-lhs>` : Parsley[AssignLHS] =
    `<deref>` <|>
    `<pair-elem>` <|>
    attempt(`<array-elem>`) <|>
    `<ident>`
  
  val `<assign-rhs>` : Parsley[AssignRHS] = 
    `<heap>` <|>
    `<pair-elem>` <|>
    `<expr>` <|>
    `<array-liter>` <|>
    "newpair" *> parens(
        lift2(NewPair, `<expr>`, "," *> `<expr>`)
    ) <|>
    ("call" *> lift2(
        Call, 
        `<ident>`,
        ArgList <#> parens(sepBy(`<expr>`, ","))
    ))

  private val `<heap>` : Parsley[Heap] =
    Malloc <#> ("malloc" *> parens(`<expr>`)) <|> 
    "realloc" *> parens(lift2(Realloc, `<ident>`, "," *> `<expr>`)) <|> 
    "calloc" *> parens(lift2(Calloc, `<expr>`, "," *> `<expr>`))
  
  private val `<deref>` : Parsley[DerefPointer] =
    "~" *> (DerefPointer <#> `<expr>`)
  
  private val `<param>` = 
    lift2(Param, `<type>`, `<ident>`)
  
  private val `<array-elem>` : Parsley[ArrayElem] = 
    lift2(ArrayElem, `<ident>`, some(brackets(`<expr>`)))

  lazy val `<ident>` : Parsley[Ident] = Ident <#> amend {IDENTIFIER}
  
  private val `<array-liter>` : Parsley[ArrayLiter] = 
    ArrayLiter <#> brackets(sepBy(`<expr>`, ","))
  
  private val `<pair-liter>` : Parsley[PairLiter] = 
    "null" #> PairLiter()
  
  private val `<program>` : Parsley[WaccProgram] = 
    programfully(
        "begin".explain("every program must start with \"begin\"") *> 
        lift2(WaccProgram, many(attempt(`<func>`)), sepBy1(`<stat>`, ";")) <* 
        "end".explain("every program must terminate with \"end\"")
    )

  private lazy val `<func>` : Parsley[Func] = 
    lift4(
        Func,
        `<type>`,
        `<ident>`,
        parens(lift1(ParamList, sepBy(`<param>`, ","))),
        ("is" *> sepBy1(`<stat>`, ";") <* "end")
    ).label("function")
  
  lazy val atom: Parsley[Expr] =
    `<ident>` <|>
    `<pair-liter>` <|>
    `<bool-liter>` <|>
    `<int-liter>` <|>
    fully(`<char-liter>` <|>
    `<str-liter>`)
  
  private lazy val `<expr>` : Parsley[Expr] =
    precedence(
      Atoms(parens(`<expr>`) <|> attempt(`<array-elem>`) <|> atom) :+
      Ops(Prefix)(
          "~".label("operator") #> DerefPointer,
          "!".label("operator") #> Not,
          notFollowedBy(`<int-liter>`) *> "-".label("operator") #> Negation,
          "len".label("operator") #> Len,
          "ord".label("operator") #> Ord,
          "chr".label("operator") #> Chr
      ) :+
      Ops(InfixL)(
          "*".label("operator") #> Mul,
          "/".label("operator") #> Div,
          "%".label("operator") #> Mod
      ) :+
      Ops(InfixL)(
          "+".label("operator") #> Plus,
          "-".label("operator") #> Sub
      ) :+

      Ops(NonAssoc)(
          (">=".label("operator") #> GTE), (">".label("operator") #> GT),
          ("<=".label("operator") #> LTE), ("<".label("operator") #> LT)
      ) :+ 

      Ops(InfixL)(
          ("==".label("operator") #> Equal),
          ("!=".label("operator") #> NotEqual)
      ) :+

      Ops(InfixL)(
          ("&&".label("operator") #> And),
          ("||".label("operator") #> Or)
      )
    ).label("expression")
    .explain("An expression can be either a literal (integer, boolean, character, string, or pair) \n" +
      "a variable, an array element, a unary expression, a binary expression or an expression \n" +
      "enclosed by a parenthesis.")

  private val skipStat : Parsley[Stat] = 
    "skip"#> Skip
  
  private val typeAssignStat : Parsley[Stat] = 
    lift3(
        TypeAssign,
        `<type>`,
        `<ident>`,
        "=" *> `<assign-rhs>`
    )
  
  private val assignLRStat : Parsley[Stat] = 
    lift2(
        AssignLR, 
        `<assign-lhs>`,
        "=" *> `<assign-rhs>`
    )
  
  private val readStat : Parsley[Stat] = 
    Read <#> "read" *> `<assign-lhs>`
  
  private val freeStat : Parsley[Stat] = 
    Free <#> "free" *> `<expr>`
  
  private val returnStat : Parsley[Stat] = 
    Return <#> "return" *> `<expr>`
  
  private val exitStat : Parsley[Stat] = 
    Exit <#> "exit" *> `<expr>`
  
  private val printStat : Parsley[Stat] = 
    Print <#> "print" *> `<expr>`
  
  private val printlnStat : Parsley[Stat] = 
    Println <#> "println" *> `<expr>`
  
  private val ifStat : Parsley[Stat] = 
    ("if" *> lift3(
        If,
        `<expr>`,
        "then" *> sepBy1(`<stat>`, ";"),
        "else" *> sepBy1(`<stat>`, ";"),
    ) <* "fi")
  
  private val whileStat : Parsley[Stat] = 
    ("while" *> lift2(
        While,
        `<expr>`,
        "do" *> sepBy1(`<stat>`, ";"),
    ) <* "done")
  
  private val beginStat : Parsley[Stat] = 
    Begin <#> ("begin" *> sepBy1(`<stat>`, ";") <* "end")

  private val `<stat>`: Parsley[Stat] =
    (assignLRStat <|>
    typeAssignStat <|>
    skipStat <|>
    whileStat <|>
    beginStat <|>
    ifStat <|>
    freeStat <|>
    exitStat <|>
    readStat <|>
    returnStat <|>
    printlnStat <|>
    printStat).label("statement")

}
