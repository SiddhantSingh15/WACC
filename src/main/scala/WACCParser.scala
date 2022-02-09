package compiler

import parsley.Parsley, Parsley._
import scala.language.implicitConversions
import Ast._
import lexer._
import parsley.character.{anyChar, char, digit, letter, noneOf, oneOf, upper, whitespace}
import parsley.combinator.{between, decide, many, manyN, option, optional, sepBy1, sepBy, some, skipMany, skipSome, sepEndBy, sepEndBy1, endBy, manyUntil}
import parsley.lift.{lift1, lift2, lift3, lift4}
import parsley.expr.{Atoms, GOps, Levels, InfixL, InfixR, NonAssoc, Ops, Prefix, SOps, Postfix, chain, precedence}
import parsley.implicits.character.{charLift, stringLift}
import parsley.lift.{lift2, lift4}
import scala.language.postfixOps
import parsley.errors.ErrorBuilder
import parsley.Result
import parsley.debug._
import parsley.expr.chain.postfix1
import parsley.combinator.attemptChoice
import parsley.io.ParseFromIO
import javax.lang.model.element.NestingKind
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
        fully((attempt("pair") #> Pair) <|> attempt(lift1(ArrayType, `<type>`)) <|> attempt(`<base-type>`))

    private lazy val `<pair-type>` : Parsley[PairType] =
        ("pair" *> parens(
            lift2(PairType, `<pair-elem-type>`,"," *> `<pair-elem-type>`)
        ))

    private val number = INTEGER

    private val intSign = "+" #> identity[Int] _ <|> "-" #> ((x: Int) => -x)
    
    private val `<int-liter>` : Parsley[IntLiter] = 
        fully(lift1(IntLiter, intSign <*> number <|> number))
    
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
        attempt(`<pair-elem>` <|>
        attempt(`<array-elem>`) <|>
        attempt(`<ident>`)))
    
    private val `<assign-rhs>` : Parsley[AssignRHS] = 
        fully(attempt(`<pair-elem>`) <|>
        attempt(`<expr>`) <|>
        attempt(`<array-liter>`) <|>
        attempt("newpair" *> parens(
            lift2(NewPair, `<expr>`, lexeme(",") *> `<expr>`)
        )) <|>
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
        ArrayLiter <#> brackets(attempt(sepBy(`<expr>`, ",")))
    
    private val `<pair-liter>` : Parsley[PairLiter] = 
        "null" #> PairLiter()
    
    private val `<program>` : Parsley[WaccProgram] = 
        fully(lexeme("begin") *> lexeme(lift2(WaccProgram, many(attempt(`<func>`)), `<stat>`)) <* "end")

    private lazy val `<func>` : Parsley[Func] = 
        lift4(
            Func,
            `<type>`,
            `<ident>`,
            parens(lift1(ParamList, sepBy(`<param>`, ","))),
            ("is" *> `<stat>` <* "end")

        )
    
    lazy val atom: Parsley[Expr] =
        fully(attempt(`<int-liter>`) <|>
        attempt(`<bool-liter>`) <|>
        attempt(`<char-liter>`) <|>
        attempt(`<str-liter>`) <|>
        attempt(`<pair-liter>`) <|>
        attempt(`<ident>`))
    
    private lazy val `<expr>` : Parsley[Expr] =
        fully(precedence(
            Atoms(fully(parens(`<expr>`) <|> attempt(`<array-elem>`) <|> atom)) :+
            Ops(Prefix)(
                attempt("!" #> Not),
                attempt(notFollowedBy(`<int-liter>`) *> "-" #> Negation),
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
                attempt(">=" #> GTE), (">" #> GT),
                attempt("<=" #> LTE), ("<" #> LT)
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
            fully("=") *> `<assign-rhs>`
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
        Exit <#> lexeme("exit") *> `<expr>`
    
    private val printStat : Parsley[Stat] = 
        Print <#> lexeme("print") *> `<expr>`
    
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
        fully(attempt(printlnStat) <|>
        attempt(assignLRStat) <|>
        attempt(skipStat) <|>
        attempt(whileStat) <|>
        attempt(beginStat) <|>
        attempt(ifStat) <|>
        attempt(typeAssignStat) <|>
        attempt(readStat) <|>
        attempt(freeStat) <|>
        attempt(returnStat) <|>
        attempt(exitStat) <|>
        attempt(printStat))

    private lazy val `<stat>` : Parsley[Stat] =
        fully(precedence(
            Atoms(atomStat) :+
            SOps(InfixR)(
                ";" #> Colon
            )
        ))


}