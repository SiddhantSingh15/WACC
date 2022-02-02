import parsley.Parsley, Parsley._
import scala.language.implicitConversions


object lexer {
    import parsley.token.{Lexer, Predicate, LanguageDef}
    import parsley.character.{alphaNum, upper, lower, newline}
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
        identStart = Predicate((c : Char) => c.isLetter || c.equals('_'))
        identLetter  = Predicate((c : Char) => _.isAlphaNum || _.equals('_'))
        space = Predicate(isWhitespace)
    )

    private val lexer = new Lexer(wacc)

    def fully[A](p : =>Parsley[A]): Parsley[A] = 
       lexer.whiteSpace *> p <* eof

    val CON_ID = idMaker(upper)
    val VAR_ID = idMaker(lower).filterOut {
        case k if wacc.keywords(k) => s"keyword $k cannot be used as an identifier"
    }

    val INTEGER = lexer.natural
    val STRING = lexer.stringLiteral
    val CHAR = lexer.charLiteral
    val IDENTIFIER = lexer.identifier
    val NEWLINE = void(lexer.lexeme(newline))

    object implicits {
        implicit def implicitToken(s : String): Parsley[Unit] = {
            if (wacc.keywords(s))        lexer.keyword(s)
            else if(wacc.operators(s))   lexer.maxOp(s)
            else                         void(lexer.symbol_(s))
        }
    }
}
