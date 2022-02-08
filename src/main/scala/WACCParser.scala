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

    def parse[Err: ErrorBuilder](input: String): Result[Err, WaccProgram] = `<program>`.debug("program").parse(input)
    def parseFromFile[Err: ErrorBuilder](file: File): Try[Result[Err, WaccProgram]] = `<program>`.debug("program").parseFromFile(file)
    
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
        fully((attempt("pair") #> Pair) <|> attempt(lift1(ArrayType, `<type>`.debug("pairelemtypetype"))) <|> attempt(`<base-type>`))

    private lazy val `<pair-type>` : Parsley[PairType] =
        ("pair" *> parens(
            lift2(PairType, `<pair-elem-type>`,",".debug("fkingcomma") *> `<pair-elem-type>`.debug("secondpairelem"))
        ))

    val number = INTEGER

    val intSign = "+" #> identity[Int] _ <|> "-" #> ((x: Int) => -x)
    
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
    
    private val `<arg-list>` : Parsley[ArgList] = 
        ArgList <#> sepBy1(`<expr>`, ",")
    
    private val `<pair-elem>` : Parsley[PairElem] = 
        (Fst <#> (attempt("fst").debug("first") *> `<expr>`.debug("firstExpr"))) <|>
        (Snd <#> (attempt("snd") *> `<expr>`))
    
    private val `<assign-lhs>` : Parsley[AssignLHS] = // TODO 
        fully(
        attempt(`<pair-elem>`.debug("pairElemAssignLHS") <|>
        attempt(`<array-elem>`).debug("arrayelemLHS") <|>
        attempt(`<ident>`).debug("identityLHS")))
    
    private val `<assign-rhs>` : Parsley[AssignRHS] = 
        fully(attempt(`<pair-elem>`.debug("pairFromRHS")) <|>
        attempt(`<expr>`) <|>
        attempt(`<array-liter>`) <|>
        attempt("newpair".debug("newpairKeyword") *> parens(
            lift2(NewPair, `<expr>`.debug("newpairexpr1"), lexeme(",") *> `<expr>`.debug("newpairexpr2"))
        )) <|>
        ("call" *> lift2(
            Call, 
            `<ident>`,
            decide(parens(option(`<arg-list>`)), empty) // Returns empty if there is no arg list
        )))
    
    private val `<param>` = 
        lift2(Param, `<type>`, `<ident>`)
    
    // private val `<param-list>` =
    //     ParamList <#> sepBy1(`<param>`, ",")
    
    private val `<array-elem>` : Parsley[ArrayElem] = 
        lift2(ArrayElem, `<ident>`, some(brackets(`<expr>`)))

    lazy val `<ident>` : Parsley[Ident] = Ident <#> fully(IDENTIFIER)
    
    private val `<array-liter>` : Parsley[ArrayLiter] = 
        ArrayLiter <#> brackets(attempt(sepBy(`<expr>`, ",").debug("arrayelem")))
    
    private val `<pair-liter>` : Parsley[PairLiter] = 
        "null" #> PairLiter()
    
    private val `<program>` : Parsley[WaccProgram] = 
        fully(lexeme("begin") *> lexeme(lift2(WaccProgram, many(attempt(`<func>`.debug("func"))), `<stat>`.debug("stat"))) <* "end".debug("end"))

    private lazy val `<func>` : Parsley[Func] = 
        lift4(
            Func,
            `<type>`.debug("type"),
            `<ident>`.debug("ident"),
            parens(lift1(ParamList, sepBy(`<param>`, ","))),
            ("is" *> `<stat>`.debug("stat") <* "end")

        )
    
    lazy val atom: Parsley[Expr] =
        fully(attempt(`<int-liter>`).debug("int-liter") <|>
        attempt(`<bool-liter>`) <|>
        attempt(`<char-liter>`.debug("char-liter")) <|>
        attempt(`<str-liter>`).debug("str-liter") <|>
        attempt(`<pair-liter>`).debug("pair-liter") <|>
        attempt(`<ident>`).debug("ident-atom"))
    
    private lazy val `<expr>` : Parsley[Expr] =
        fully(precedence(
            Atoms(fully(parens(`<expr>`) <|> attempt(`<array-elem>`) <|> atom)) :+ // can consider replacing :+ with ,
            Ops(Prefix)(
                attempt("!".debug("exclamation") #> Not),
                attempt(notFollowedBy(`<int-liter>`) *> "-" #> Negation),
                attempt("len" #> Len),
                attempt("ord" #> Ord),
                attempt("chr" #> Chr)
            ) :+
            Ops(InfixL)(
                ("*").debug("multiply") #> Mul,
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
        "skip".debug("skip2") #> Skip
    
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
            `<assign-lhs>`.debug("assignLHS"),
            "=".debug("equalsSign") *> `<assign-rhs>`.debug("assignRHS")
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
        Println <#> "println" *> `<expr>`.debug("HELP")
    
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
        fully(attempt(printlnStat.debug("println")) <|>
        attempt(assignLRStat.debug("assignLRStatreallylongdebugstatement")) <|>
        attempt(skipStat) <|>
        attempt(whileStat.debug("while")) <|>
        attempt(beginStat) <|>
        attempt(ifStat) <|>
        attempt(typeAssignStat) <|>
        attempt(readStat) <|>
        attempt(freeStat) <|>
        attempt(returnStat) <|>
        attempt(exitStat) <|>
        attempt(printStat))

    private lazy val `<stat>` : Parsley[Stat] = // not sure whats the associativity of a while statement
        fully(precedence(
            Atoms(atomStat) :+
            SOps(InfixR)(
                ";".debug("sth") #> Colon
            )
        ))


}