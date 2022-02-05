import parsley.Parsley
import parsley.Parsley._
import parsley.character.{char, digit, letter, noneOf, oneOf, upper, whitespace}
import parsley.combinator.{between, many, manyN, option, sepBy1}
import parsley.lift.{lift2, lift3, lift4}
import parsley.expr.{InfixL, Ops, Postfix, Prefix, precedence}
import parsley.token.{LanguageDef, Lexer}



import parsley.Parsley._
import parsley.implicits.character.{charLift, stringLift}
import parsley.{Parsley, Result}
import parsley.character.{anyChar, char, digit, letter, noneOf, upper}
import parsley.combinator.{between, many, option, sepBy1}
import parsley.lift.{lift2, lift4}
import Ast._
import parsley.expr.{GOps, Levels, Ops, Postfix, Prefix, chain, precedence}
import parsley.expr.{precedence, SOps, InfixL, InfixR, NonAssoc, Prefix, Atoms}
import parsley.combinator.sepBy1
import scala.language.postfixOps

object Parser {
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
      ("char" #> Char) <|> 
      ("string" #> String)  
    
    private val `<type>` : Parsley[Type] = 
        precedence[Type](
            `<base-type>` <|> 
            `<pair-type>`,
            Ops[Type](Postfix)("[]") #> ArrayType
        )   
    
    private val `<pair-elem-type>`: Parsley[PairElemType] =
        precedence[PairElemType](
            `<base-type>`,
            `<array-type>`
        )

    private lazy val `<pair-type>` : Parsley[PairType] =
        ("pair" *> lexer.parens(
            lift2(Pair, `<pair-elem-type>`, "," *> `<pair-elem-type>`)
        ))
    

    private val `<unary-oper>`: Parsley[UnOp] =
      ('!' #> Not) <|> 
      ('-' #> Negation) <|> 
      ("len" #> Len) <|> 
      ("ord" #> Ord) <|> 
      ("chr" #> Chr)    
    
    private val `<binary-oper>`: Parsley[BinOp] =
      ('*' #> Mul) <|> 
      ('/' #> Div) <|> 
      ('%' #> Mod) <|> 
      ('+' #> Plus) <|>
      ('-' #> Sub) <|> 
      ('>' #> GT) <|> 
      (">=" #> GTE) <|> 
      ('<' #> LT) <|> 
      ("<=" #> LTE) <|> 
      ("==" #> Equal) <|> 
      ("!=" #> NotEqual) <|> 
      ("&&" #> And) <|> 
      ("||" #> Or)  
    
    private val `<digit>`: Parsley[Int] =
      digit.foldLeft1[Int](0)((n, d) => n * 10 + d.asDigit) 
    
    private val `<int-sign>`: Parsley[IntSign] = 
      ('+' #> Pos) <|> ('-' #> Neg) 
    
    private val `<int-liter>` : Parsley[IntLiter] =
        (option(lookAhead(`<int-sign>`) <~> lexer.integer))
        .map(
            (a : (Option[IntSign], Int)) => a._2
        )
    
    private val `<char-liter>` : Parsley[CharLiter] = 
       (CharLiter <#>  "\'" *> `<character>` <* "\'")

    private val `<str-liter>` : Parsley[StrLiter] =
        (StrLiter <#> '\"' *> many(`<character>`) <* '\"') 
        
    
    private val `<escaped-char>` : Parsley[EscapedChar] = 
        oneOf(
            Set('0','b','t','n','f','r','"','"','\'')
        )
    
    private val `<character>` : Parsley[Char] = 
        (EscapedChar <#> "\\" *> EscapedChar) <|>
        (Char <#> noneOf('\\', '\'', '"'))
    
    
    private val `<bool-liter>`: Parsley[BoolLiter] = 
      ("true" #> True) <|> ("false" #> False)  
    
    private val `<arg-list>` : Parsley[ArgList] = 
        ArgList <#> sepBy1(`<expr>`, ",")
    
    private val `<pair-elem>` : Parsley[PairElem] = 
        Fst <#> "fst" #> `<expr>` <|>
        Snd <#> "snd" #> `<expr>`
    
    private val `<assign-lhs>` : Parsley[AssignLHS] = 
        `<pair-elem>` <|>
        `<array-elem>` <|>
        `<ident>`
    
    private val `<assign-rhs>` : Parsley[AssignRHS] = 
        `<expr>` <|>
        `<array-liter>` <|>
        ("newpair" *> lexer.parens(
            lift2(NewPair, `<expr>`, "," *> `<expr>`)
        )) <|>
        ("call" *> lift2(
            Call, 
            `<ident>`,
            lexer.parens(option(`<arg-list>`))
        ))
    
    private val `<param>` = 
        lift2(Param, `<type>`, `<ident>`)
    
    private val `<param-list>` =
        ParamList <#> sepBy1(`<param>`, ",")
    
    private val `<array-elem>` : Parsley[ArrayElem] = 
        lift2(
            ArrayElem, 
            `<ident>`,
            manyN(1, lexer.brackets(`<expr>`))
        )

    lazy val `<ident>` : Parsley[Ident] = Ident <#> lexer.IDENTIFIER
    
    private val `<array-liter>` : Parsley[ArrayElem] = 
        ArrayLiter <#> lexer.brackets(
            option(sepBy1(`<expr>`, ","))
        )
    
    private val `<pair-liter>` : Parsley[PairLiter] = 
        "null" #> PairLiter()
    
    private val `<program>` : Parsley[WaccProgram] = 
        (WaccProgram <#> "begin" *> lift2(WaccProgram, many(attempt(func)), stat) <* "end") 

    private val `<func>` : Parsley[Func] = 
        lift4(
            Func,
            `<type>`,
            `<ident>`,
            lexer.parens(option(`<param-list>`)),
            ("is" #> `<stat>` <# "end")

        )
    
    private lazy val `<expr>` : Parsley[Expr] = 
        `<int-liter>` <|>
        `<bool-liter>` <|>
        `<char-liter>` <|>
        `<str-liter>` <|>
        `<pair-liter>` <|>
        `<ident>` <|>
        `<array-elem>` <|>
        (Parens <#> lexer.parens(`<expr>`)) <|>
        lift2(Expr, `<unary-oper>`, `<expr>`) <|>
        lift3(Expr, `<expr>`, `<unary-oper>`, `<expr>`)
        
    private val skipStat : Parsley[Stat] = 
        "skip" #> Skip
    
    private val typeAssignStat : Parsley[Stat] = 
        lift3(
            TypeAssign,
            `<type>`,
            `<ident>`,
            "=" #> `<assign-rhs>`
        )
    
    private val assignLRStat : Parsley[Stat] = 
        lift2(
            AssignLR, 
            `<assign-lhs>`,
            "=" #> `<assign-rhs>`
        )
    
    private val readStat : Parsley[Stat] = 
        Read <#> "read" *> `<assign-lhs>`
    
    private val freeStat : Parsley[Stat] = 
        Free <#> "free" *> `<expr>`
    
    private val returnStat : Parsley[Stat] = 
        Return <#> "return" #> `<expr>`
    
    private val exitStat : Parsley[Stat] = 
        Exit <#> "exit" #> `<expr>`
    
    
    private val printStat : Parsley[Stat] = 
        Print <#> "print" #> `<expr>`
    
    private val printlnStat : Parsley[Stat] = 
        Println <#> "println" #> `<expr>`
    
    private val ifStat : Parsley[Stat] = 
        ("if" *> lift3(
            If,
            `<expr>`,
            "then" *> `<stat>`,
            "else" *> `<stat>`,
        ) <* "fi")
    
    private val whileStat : Parsley[Stat] = 
        ("while" *> lift2(
            Whiel,
            "while" *> `<expr>`,
            "do" *> `<stat>`,
        ) <* "done")
    
    private val beginStat : Parsley[Stat] = 
        "begin" #> `<stat>` <# "end"

    private val colonStat : Parsley[Stat] = 
        `<stat>`<#> ";" <#> `<stat>`

    private lazy val `<stat>` : Parsley[Stat] = 
        skipStat <|> 
        typeAssignStat <|>
        assignLRStat <|>
        readStat <|>
        freeStat <|>
        readStat <|>
        returnStat <|>
        exitStat <|>
        printStat <|>
        printlnStat <|>
        ifStat <|>
        whileStat <|>
        beginStat <|>
        colonStat


}