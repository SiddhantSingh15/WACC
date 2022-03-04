package frontend

import parsley.Parsley, Parsley._
import scala.language.implicitConversions


object lexer {
    import parsley.token.{Lexer, Predicate, LanguageDef}
    import parsley.character.{alphaNum, upper, lower, newline}
    import parsley.character.{char, digit, isWhitespace, letter, upper}
    import parsley.combinator.{many, option, eof}
    import parsley.implicits.character.{stringLift, charLift}
    import parsley.implicits.zipped.Zipped3
    import parsley.errors.combinator._

    val keywords = Set("begin", "end", "is", "skip", "read", "free",
                    "return", "exit", "print", "println", "if", 
                    "then", "else", "fi", "while", "do", "done",
                    "begin", "end", "newpair", "call", "fst", "snd", 
                    "newpair", "pair", "null", "true", "false", "int",
                    "char", "string", "bool", "len", "ord", "chr"
                    )
                    
    val operators = Set("!", "-", "len", "ord", "chr", "*",
                "/", "%", "+", "-", ">", ">=", "<",
                "<=", "==", "!=", "&&", "||", "fst", "snd")

    private val wacc = LanguageDef.plain.copy(
        commentLine = "#",
        nestedComments = false,
        keywords = this.keywords,
        operators = this.operators,
        identStart = Predicate(c => (c == '_' || c.isLetter)),
        identLetter  = Predicate(c => (c == '_' || c.isLetterOrDigit)),
        space = 
            Predicate(c => c == ' ' || c == '\t' || c == '\n' || c == '\u000d')
    )

    private val lexer = new Lexer(wacc)

    def parens[A](p: => Parsley[A]): Parsley[A] = lexer.parens(p)

    def brackets[A](p: => Parsley[A]): Parsley[A] = lexer.brackets(p)

    def programfully[A](p : =>Parsley[A]): Parsley[A] =
        lexer.whiteSpace *> p <* eof.label("end of input")
        .explain("WACC file should contain only one WACC program")
        .explain("Extra code detected after \"end\"")

    def fully[A](p : =>Parsley[A]): Parsley[A] = 
       lexer.whiteSpace *> p <* lexer.whiteSpace

    val IDENTIFIER = amend {lexer.identifier}.label("identifier")
    val NUMBER = 
        lexer
        .lexeme(digit.label("end of number")
                    .foldLeft1[Long](0)((n, d) => n * 10 + d.asDigit))
        .label("number")

    object implicits {
        implicit def implicitToken(s : String): Parsley[Unit] = {
            if (wacc.keywords(s))        lexer.keyword(s)
            else if(wacc.operators(s))   lexer.maxOp(s)
            else                         void(lexer.symbol_(s))
        }
    }
}
