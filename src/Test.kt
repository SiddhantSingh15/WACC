// import ANTLR's runtime libararies
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;

// import antlr package (your code)
import antlr.*;

fun main(){

	//create  a CharStream that reads from standard input
	val input = CharStreams.fromStream(System.`in`);

	//create a lexer that feeds off of input CharStream
	val lexer = BasicLexer(input);

	//create a buffer of tokens pulled from the lexer
	val tokens = CommonTokenStream(lexer);

	//create a parser that feeds off the tokens buffer
	val parser = BasicParser(tokens);

	val tree = parser.prog(); //begin parsing at prog rule
	
	println(tree.toStringTree(parser)); //print LISP-style tree

}
