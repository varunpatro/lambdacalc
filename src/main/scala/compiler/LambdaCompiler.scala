import lexer.LambdaLexer
import parser.{LambdaAST, LambdaParser}

object LambdaCompiler {
  def apply(code: String): Option[LambdaAST] = {
    val tokens = LambdaLexer(code)
    val ast = LambdaParser(tokens)
    return ast
  }
}
