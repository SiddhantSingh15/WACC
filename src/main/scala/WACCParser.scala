package compiler

import parsley.Parsley, Parsley._
import scala.language.implicitConversions
import Ast._
import lexer._
import parsley.character.{anyChar, char, digit, letter, noneOf, oneOf, upper, whitespace}
import parsley.combinator.{between, decide, many, manyN, option, optional, sepBy1, sepBy, some, skipMany, skipSome, sepEndBy, sepEndBy1, endBy, manyUntil}
import parsley.lift.{lift2, lift3, lift4}
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

    private def terminationRules(stat: Stat): Boolean = stat match {
        case Begin(stat)         => terminationRules(stat)
        case While(_, stat)      => terminationRules(stat)
        case If(_, stat1, stat2) => terminationRules(stat1) && terminationRules(stat2)
        case Exit(_)             => true
        case Return(_)           => true
        case _                   => false
      }

    
    private lazy val `<base-type>`: Parsley[BaseType] = 
      ("int" #> Int) <|> 
      ("bool" #> Bool) <|> 
      ("char" #> CharType) <|> 
      ("string" #> String)  
    
    // private lazy val `<type>` : Parsley[Type] = 
        // precedence[Type](`<base-type>`.debug("basetype"))(
            // Ops[Type](Postfix)(`<pair-type>`),
            // Ops[Type](Postfix)(`<array-type>`)
        // )


    // private lazy val `<type>` : Parsley[Type] = 
    //     attempt(chain.postfix1(`<base-type>` <|> `<pair-type>`, ArrayType <#> ( _ <* '[' <* ']'))) <|> `<base-type>` <|> (NestedPairType <# "pair")

    private lazy val `<type>` : Parsley[Type] =
        fully(precedence(`<base-type>`, `<pair-type>`)(Ops(Postfix)("[]" #> ArrayType)))

    // private val `<array-type>` : Parsley[ArrayType] =
    //     chain.postfix1(`<type>`,"[]" #> ArrayType)
        
    // private lazy val `<pair-elem-type>`: Parsley[PairElemType] =
    //     attemptChoice(`<base-type>`, `<array-type>`, )
    
    // private lazy val `<pair-elem-type>`: Parsley[PairElemType] =
        // attempt(chain.postfix(`<base-type>` <|> `<pair-type>`,
                            // ArrayType #> ('[' <* ']'))) <|> `<base-type>` <|> 

    // private lazy val `<pair-elem-type>`: Parsley[PairElemType] = 
    //     attempt(chain.postfix1(`<base-type>` <|> `<pair-type>`, ArrayType <# "[]")) <|> `<base-type>` <|> (NestedPairType <# "pair")

    private lazy val `<pair-elem-type>`: Parsley[PairElemType] =
            `<base-type>` <|> ("pair" #> Pair)

    private lazy val `<pair-type>` : Parsley[PairType] =
        ("pair" *> parens(
            lift2(PairType, `<pair-elem-type>`, lexeme(",") *> `<pair-elem-type>`)
        ))
    

    // private val `<unary-oper>`: Parsley[UnOp] =
    //   ('!' #> Not) <|> 
    //   ('-' #> Negation) <|> 
    //   ("len" #> Len) <|> 
    //   ("ord" #> Ord) <|> 
    //   ("chr" #> Chr)    
    
    // private val `<binary-oper>`: Parsley[BinOp] =
    //   ('*' #> Mul) <|> 
    //   ('/' #> Div) <|> 
    //   ('%' #> Mod) <|> 
    //   ('+' #> Plus) <|>
    //   ('-' #> Sub) <|> 
    //   ('>' #> GT) <|> 
    //   (">=" #> GTE) <|> 
    //   ('<' #> LT) <|> 
    //   ("<=" #> LTE) <|> 
    //   ("==" #> Equal) <|> 
    //   ("!=" #> NotEqual) <|> 
    //   ("&&" #> And) <|> 
    //   ("||" #> Or)  
    
    private val `<digit>`: Parsley[Digit] =
    //   Digit <#> digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit) 
        Digit <#> INTEGER
    private val `<int-sign>`: Parsley[IntSign] = 
      ('+' #> Pos) <|> ('-' #> Neg) 
    
    private val `<int-liter>` : Parsley[IntLiter] =  // should make it such that even without an IntSign, we give it the positive one as default
        // (option(lookAhead(`<int-sign>`) <~> INTEGER))
        // .map(
        //     (a : (Option[IntSign], Int)) => a._2
        // )
        lift2(IntLiter, `<int-sign>`, manyN(1, `<digit>`)) <|> lift2(IntLiter, pure(Pos), manyN(1, `<digit>`.debug("digit")))
    
    private val `<char-liter>` : Parsley[CharLiter] = 
       (CharLiter <#>  "\'" *> `<character>` <* "\'")

    private val `<str-liter>` : Parsley[StrLiter] =
        (StrLiter <#> '\"' *> many(`<character>`) <* '\"') 
        
    
    private val `<escaped-char>` : Parsley[EscapedChar] =  // Chose to remove EscapedChar cause I cant figure out a better way
        EscapedChar <#> oneOf(
            Set('0','b','t','n','f','r','"','"','\'')
        )
    
    private val `<character>` : Parsley[Character] = // There's a difference between char and character and char liter
        (("\\" *> `<escaped-char>`)) <|>
        (ASCIIChar <#> noneOf(Set('\\', '\'', '"')))
    
    
    private val `<bool-liter>`: Parsley[BoolLiter] = 
      ("true" #> True) <|> ("false" #> False)  
    
    private val `<arg-list>` : Parsley[ArgList] = 
        ArgList <#> sepBy1(`<expr>`, ",")
    
    private val `<pair-elem>` : Parsley[PairElem] = 
        (Fst <#> ("fst".debug("first") *> `<expr>`.debug("firstExpr"))) <|>
        (Snd <#> ("snd" *> `<expr>`))
    
    private val `<assign-lhs>` : Parsley[AssignLHS] = // TODO 
        fully(attempt(`<pair-elem>`.debug("pairElemAssignLHS")) <|>
        attempt(`<array-elem>`) <|>
        attempt(`<ident>`))
    
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
    
    private val `<param-list>` =
        ParamList <#> sepBy1(`<param>`, ",")
    
    private val `<array-elem>` : Parsley[ArrayElem] = 
        lift2(ArrayElem, `<ident>`, manyN(1, brackets(`<expr>`.debug("exprInArrayElem"))))

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
            parens(`<param-list>`.debug("param-list") <|> empty), // Returns empty if nothing in param-list
            ("is" *> `<stat>`.debug("stat") <* "end")

        )
    
    lazy val atom: Parsley[Expr] =
        fully(attempt(`<array-elem>`.debug("array-elem")) <|>
        attempt(`<int-liter>`).debug("int-liter") <|>
        attempt(`<bool-liter>`) <|>
        attempt(`<str-liter>`) <|>
        attempt(`<char-liter>`.debug("char-liter")) <|>
        attempt(`<pair-liter>`) <|>
        attempt(`<ident>`.debug("ident")) <|>
        attempt(parens(`<expr>`)))
    
    private lazy val `<expr>` : Parsley[Expr] =
        fully(precedence(
            Atoms(atom.debug("atom")) :+
            SOps(Prefix)(
                attempt("!".debug("exclamation") #> Not),
                attempt(notFollowedBy(`<int-liter>`) *> "-" #> Negation),
                attempt("len" #> Len),
                attempt("ord" #> Ord),
                attempt("chr" #> Chr)
            ) :+

            SOps(InfixL)(
                "*" #> Mul,
                "/" #> Div,
                "%" #> Mod
            ) :+
            SOps(InfixL)(
                "+" #> Plus,
                "-" #> Sub
            ) :+

            SOps(InfixL)(
                (">" #> GT) <|> (">=" #> GTE),
                ("<" #> LT) <|> ("<=" #> LTE)
            ) :+      

            SOps(InfixL)(
                ("==" #> Equal),
                ("!=" #> NotEqual)
            ) :+

            SOps(InfixL)(
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
            fully("=").debug("equalsSign") *> `<assign-rhs>`.debug("assignRHS")
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
        attempt(skipStat) <|>
        attempt(whileStat.debug("while")) <|>
        attempt(beginStat) <|>
        attempt(ifStat) <|>
        attempt(assignLRStat.debug("assignLRStatreallylongdebugstatement")) <|>
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