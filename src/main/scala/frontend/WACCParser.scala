package frontend

import parsley.Parsley, Parsley._
import scala.language.implicitConversions
import Ast._
import lexer._
import parsley.character.{noneOf, oneOf}
import parsley.combinator.{many, sepBy, some, sepEndBy}
import parsley.lift.{lift1, lift2, lift3, lift4}
import parsley.expr.{Atoms, InfixL, InfixR, NonAssoc, Ops, Prefix, SOps, Postfix, chain, precedence}
import parsley.implicits.character.{charLift, stringLift}
import parsley.errors.ErrorBuilder
import parsley.Result
import parsley.io.ParseFromIO
import java.io.File
import scala.util.Try

object Parser {

    def parse[Err: ErrorBuilder](input: String): Result[Err, WaccProgram] = `<program>`.parse(input)
    def parseFromFile[Err: ErrorBuilder](file: File): Try[Result[Err, WaccProgram]] = `<program>`.parseFromFile(file)
    
    private lazy val `<base-type>`: Parsley[BaseType] = 
      ("int" #> Int) <|> 
      ("bool" #> Bool) <|> 
      ("char" #> CharType) <|> 
      ("string" #> String)  
    
    private lazy val `<type>` : Parsley[Type] =
        fully(attempt(`<array-type>`) <|> `<base-type>` <|> `<pair-type>`)

    private lazy val `<array-type>` : Parsley[ArrayType] =
        chain.postfix1((`<base-type>` <|> `<pair-type>`), "[]" #> ArrayType)

    private lazy val `<pair-elem-type>`: Parsley[PairElemType] =
        fully("pair" #> PairElemPair <|> (PairElemWithType <#> `<type>`))

    private lazy val `<pair-type>` : Parsley[PairType] =
        ("pair" *> parens(
            lift2(Pair, `<pair-elem-type>`,"," *> `<pair-elem-type>`))
        )

    private val number = INTEGER

    private val intSign = "+" #> identity[Int] _ <|> "-" #> ((x: Int) => -x)
    
    private val `<int-liter>` : Parsley[IntLiter] = 
        lift1(IntLiter, intSign <*> number <|> number)
    
    private val `<char-liter>` : Parsley[CharLiter] = 
       (CharLiter <#>  "\'" *> `<character>` <* "\'")

    private val `<str-liter>` : Parsley[StrLiter] =
        StrLiter <#> ('\"' *> (many(`<character>`)) <* '\"').map(s => s.mkString)        
    
    private val `<escaped-char>` : Parsley[Char] = 
        oneOf(
            Set('0','b','t','n','f','r','"','"','\'')
        )
    
    private val `<character>` : Parsley[Char] =
        (("\\" *> `<escaped-char>`)) <|>
        (noneOf('\\', '\'', '"'))
    
    
    private val `<bool-liter>`: Parsley[BoolLiter] = 
      ("true" #> True) <|> ("false" #> False)  
    
    private val `<pair-elem>` : Parsley[PairElem] = 
        (Fst <#> (attempt("fst") *> `<expr>`)) <|>
        (Snd <#> (attempt("snd") *> `<expr>`))
    
    private val `<assign-lhs>` : Parsley[AssignLHS] =
        fully(
        `<pair-elem>` <|>
        attempt(`<array-elem>`) <|>
        `<ident>`)
    
    private val `<assign-rhs>` : Parsley[AssignRHS] = 
        fully(`<pair-elem>` <|>
        `<expr>` <|>
        `<array-liter>` <|>
        "newpair" *> parens(
            lift2(NewPair, `<expr>`, "," *> `<expr>`)
        ) <|>
        ("call" *> lift2(
            Call, 
            `<ident>`,
            ArgList <#> parens(sepBy(`<expr>`, ","))
        )))
    
    private val `<param>` = 
        lift2(Param, `<type>`, `<ident>`)
    
    private val `<array-elem>` : Parsley[ArrayElem] = 
        lift2(ArrayElem, `<ident>`, some(brackets(`<expr>`)))

    lazy val `<ident>` : Parsley[Ident] = Ident <#> fully(IDENTIFIER)
    
    private val `<array-liter>` : Parsley[ArrayLiter] = 
        ArrayLiter <#> brackets(sepBy(`<expr>`, ","))
    
    private val `<pair-liter>` : Parsley[PairLiter] = 
        "null" #> PairLiter()
    
    private val `<program>` : Parsley[WaccProgram] = 
        fully("begin" *> lift2(WaccProgram, many(attempt(`<func>`)), `<stat>`) <* "end")

    private lazy val `<func>` : Parsley[Func] = 
        lift4(
            Func,
            `<type>`,
            `<ident>`,
            parens(lift1(ParamList, sepBy(`<param>`, ","))),
            ("is" *> `<stat>` <* "end")

        )
    
    lazy val atom: Parsley[Expr] =
        attempt(`<ident>`) <|>
        attempt(`<pair-liter>`) <|>
        `<bool-liter>` <|>
        `<int-liter>` <|>
        `<char-liter>` <|>
        `<str-liter>`
    
    private lazy val `<expr>` : Parsley[Expr] =
        fully(precedence(
            Atoms(fully(parens(`<expr>`) <|> attempt(`<array-elem>`) <|> atom)) :+
            Ops(Prefix)(
                "!" #> Not,
                notFollowedBy(`<int-liter>`) *> "-" #> Negation,
                attempt("len" #> Len),
                attempt("ord" #> Ord),
                attempt("chr" #> Chr)
            ) :+
            Ops(InfixL)(
                "*" #> Mul,
                "/" #> Div,
                "%" #> Mod
            ) :+
            Ops(InfixL)(
                "+" #> Plus,
                "-" #> Sub
            ) :+

            Ops(NonAssoc)(
                (">" *> ("=" #> GTE <|> "" #> GT)),
                ("<" *> ("=" #> LTE <|> "" #> LT))
            ) :+      

            Ops(InfixL)(
                (attempt("==") #> Equal),
                ("!=" #> NotEqual)
            ) :+

            Ops(InfixR)(
                ("&&" #> And),
                ("||" #> Or)
            )
        ))

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
            "then" *> `<stat>`,
            "else" *> `<stat>`,
        ) <* "fi")
    
    private val whileStat : Parsley[Stat] = 
        ("while" *> lift2(
            While,
            `<expr>`,
            "do" *> `<stat>`,
        ) <* "done")
    
    private val beginStat : Parsley[Stat] = 
        Begin <#> ("begin" *> `<stat>` <* "end")

    private val colonStat : Parsley[Stat] = 
        lift2(Colon, `<stat>`, ";" *> `<stat>`)

    private val atomStat: Parsley[Stat] =
        fully(
        attempt(assignLRStat) <|>
        attempt(typeAssignStat) <|>
        skipStat <|>
        whileStat <|>
        beginStat <|>
        ifStat <|>
        freeStat <|>
        exitStat <|>
        attempt(readStat) <|>
        returnStat <|>
        attempt(printlnStat) <|>
        printStat)

    private lazy val `<stat>` : Parsley[Stat] = 
        precedence(
            Atoms(atomStat) :+
            SOps(InfixR)(
                ";" #> Colon
            )
        )


}
