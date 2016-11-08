package parser

import lexer._

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers

object LambdaParser extends Parsers {
  type Elem = LambdaToken

  def program: Parser[LambdaAST] = {
    phrase(expr)
  }

  def expr: Parser[LambdaAST] = {
    rep1(simpleExpr) ^^ { _.reduce(FApp) }
  }

  def simpleExpr: Parser[LambdaAST] = {
    ident | parenExpr | let | LAMBDA ~> fun
  }

  def ident: Parser[Var] = {
    accept("ident", { case ID(id) => Var(id) })
  }

  def parenExpr: Parser[LambdaAST] = {
    OPEN ~> expr <~ CLOSE
  }

  def let: Parser[Let] = {
    (LET ~> ident) ~ (EQUAL ~> expr) ~ (IN ~> expr) ^^ {
      case n ~ f ~ v => Let(n.toString, f, v)
    }
  }

  def fun: Parser[Fun] = {
    val p1 = ident ~ DOT ~ expr ^^ {case arg ~ _ ~ body => Fun(arg.toString, body)}
    val p2 = ident ~ fun ^^ {case arg ~ body => Fun(arg.toString, body)}
    p1 | p2
  }

  def apply(tokensOption: Option[Seq[LambdaToken]]): Option[LambdaAST] = {
    tokensOption match {
      case Some(tokens) => {
        val reader = new LambdaTokenReader(tokens)
        program(reader) match {
          case NoSuccess(msg, next) => None
          case Success(result, next) => Some(result)
        }
      }
    }
  }
}
