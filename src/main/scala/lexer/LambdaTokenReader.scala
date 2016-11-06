package lexer

import scala.util.parsing.input.{Position, Reader}

class LambdaTokenReader(tokens: Seq[LambdaToken], position: Int = 0) extends Reader[LambdaToken] {

  override def first: LambdaToken = if (position < tokens.size) tokens(position) else END
  override def atEnd: Boolean = position >= tokens.size
  override def pos: Position = new Position {
    def line = position
    def column = if (position == 0) 0 else (tokens.take(position) mkString ", ").size + 3
    def lineContents = tokens mkString ", "
  }

  override def rest: Reader[LambdaToken] = if (position < tokens.size) new LambdaTokenReader(tokens, position + 1) else this
}