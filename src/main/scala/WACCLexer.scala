package compiler

import parsley.Parsley, Parsley._
import scala.language.implicitConversions


object lexer {
    import parsley.token.{Lexer, Predicate, LanguageDef}
    import parsley.character.{alphaNum, upper, lower, newline}
    import parsley.character.{char, digit, isWhitespace, letter, upper}
    import parsley.combinator.{many, option, eof}
    import parsley.implicits.character.{stringLift, charLift}
    import parsley.implicits.zipped.Zipped3
    import parsley.errors.combinator.ErrorMethods

    val keywords = Set("begin", "end", "is", "skip", "read", "free",
                    "return", "exit", "print", "println", "if", 
                    "then", "else", "fi", "while", "do", "done",
                    "begin", "end", "newpair", "call"
                    )
                    
    val operators = Set("!", "-", "len", "ord", "chr", "*",
                "/", "%", "+", "-", ">", ">=", "<",
                "<=", "==", "!=", "&&", "||", "fst", "snd")

    private val wacc = LanguageDef.plain.copy(
        commentLine = "#",
        nestedComments = false,
        keywords = this.keywords,
        operators = this.operators,
        identStart = parsley.token.Parser(char('_') <|> letter <|> upper),
        identLetter  = parsley.token.Parser(char('_') <|> letter <|> upper <|> digit),
        space = Predicate(c => c == ' ' || c == '\t' || c == '\n' || c == '\u000d')
    )

    private val lexer = new Lexer(wacc)

    def parens[A](p: => Parsley[A]): Parsley[A] = lexer.parens(p)

    def brackets[A](p: => Parsley[A]): Parsley[A] = lexer.brackets(p)

    def fully[A](p : =>Parsley[A]): Parsley[A] = 
       lexer.whiteSpace *> p <* lexer.whiteSpace
    
    def lexeme[A](p : =>Parsley[A]): Parsley[A] = lexer.lexeme(p)

    val VAR_ID = lexer.lexeme(IDENTIFIER)

    val INTEGER = lexer.integer
    val STRING = lexer.stringLiteral
    val CHAR = lexer.charLiteral
    val IDENTIFIER = lexer.identifier
    val NEWLINE = void(lexer.lexeme(newline))
    // val WHITESPACE = lexer.whiteSpace

    object implicits {
        implicit def implicitToken(s : String): Parsley[Unit] = {
            if (wacc.keywords(s))        lexer.keyword(s)
            else if(wacc.operators(s))   lexer.maxOp(s)
            else                         void(lexer.symbol_(s))
        }
    }
}
