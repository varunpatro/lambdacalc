package parser

import lexer._

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers

object LambdaParser extends Parsers {
  type Elem = LambdaToken

  def program: Parser[LambdaAST] = {
    phrase(expr ^^ reduceFAppList)
  }

  def reduceFAppList(ast: LambdaAST): LambdaAST = {
    ast match {
      case FAppList(xs: List[LambdaAST]) => {
        xs.map(reduceFAppList).reduce(FApp)
      }
      case Let(key, value, body) => Let(key, reduceFAppList(value), reduceFAppList(body))
      case Fun(arg, body) => Fun(arg, reduceFAppList(body))
      case Var(key) => Var(key)
    }
  }

  def term: Parser[LambdaAST] = {
    ident | parenExpr | let | LAMBDA ~> fun
  }

  def expr: Parser[LambdaAST] = {
    atLeast2expr | term
  }

  def atLeast2expr: Parser[LambdaAST] = {
    val p1 = term ~ term ~ expr ^^ { case a ~ b ~ c => {
      c match {
        case FAppList(xs) => FAppList({
          val x = List(a, b)
          val y = x ++ xs
          y
        })
        case _ => FAppList(List(a, b, c))
      }
    }
    }
    val p2 = term ~ term ^^ { case a ~ b => FAppList(List(a, b)) }
    p1 | p2
  }

  def ident: Parser[Var] = {
    accept("ident", {case ID(id) => Var(id)})
  }

  def parenExpr: Parser[LambdaAST] = {
    OPEN ~> expr <~ CLOSE
  }

  def let: Parser[Let] = {
    LET ~ ident ~ EQUAL ~ expr ~ IN ~ expr ^^ {
      case _ ~ n ~ _ ~ f ~ _ ~ v => Let(n.toString, f, v)
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
