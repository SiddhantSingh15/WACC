import parsley.Parsley, Parsley._
import scala.language.implicitConversions


object lexer {
    import parsley.token.{Lexer, Predicate, LanguageDef}
    import parsley.character.{alphaNum, upper, lower, newline}
    import parsley.combinator.{many, option, eof}
    import parsley.implicits.character.{stringLift, charLift}
    import parsley.implicits.zipped.Zipped3
    import parsley.errors.combinator.ErrorMethods

    private val wacc = LanguageDef.plain.copy(
        commentLine = "#",
        nestedComments = false,
        keywords = Set("begin", "end", "is", "skip", "read", "free",
                        "return", "exit", "print", "println", "if", 
                        "then", "else", "fi", "while", "do", "done",
                        "begin", "end", "newpair", "call"
                        ),
        operators = Set("!", "-", "len", "ord", "chr", "*",
                        "/", "%", "+", "-", ">", ">=", "<",
                        "<=", "==", "!=", "&&", "||", "fst", "snd"),
        identLetter = Predicate(c => c.isLetter || c == '_'),
        space = Predicate(c => c == ' ' || c == '\t' || c == '\n')
    )

    private val lexer = new Lexer(wacc)

    private val identifierTail = many(alphaNum <|> '_')

    private def idMaker(leading : Parsley[Char]) =
        (leading, identifierTail, many('\'')).zipped {
            (first, middle, primes) => s"$first${middle.mkString}${primes.mkString}"
        }

    val CON_ID = idMaker(upper)
    val VAR_ID = idMaker(lower).filterOut {
        case k if wacc.keywords(k) => s"keyword $k cannot be used as an identifier"
    }

    val INTEGER = lexer.natural
    val STRING = lexer.stringLiteral
    val CHAR = lexer.charLiteral

    val NEWLINE = void(lexer.lexeme(newline))

    def fully[A](p : Parsley[A]): Parsley[A] = 
        lexer.whiteSpace *> p <* eof

    object implicits {
        implicit def implicitToken(s : String): Parsley[Unit] = {
            if (wacc.keywords(s))        lexer.keyword(s)
            else if(wacc.operators(s))   lexer.maxOp(s)
            else                         void(lexer.symbol_(s))
        }
    }
}
