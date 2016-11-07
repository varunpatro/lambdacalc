package parser

import lexer._

import scala.collection.mutable.ListBuffer
import scala.util.parsing.combinator.Parsers

object LambdaParser extends Parsers {
  type Elem = LambdaToken

  def program: Parser[LambdaAST] = {
    phrase(expr ^^ flattenFAppList ^^ reduceFAppFlatList)
  }

  def flattenFAppList(ast: LambdaAST): LambdaAST = {
    ast match {
      case FAppList(xs: List[LambdaAST]) => {
        if (xs.size == 2) {
          val f = flattenFAppList(xs(0))
          val v = flattenFAppList(xs(1))
          FAppFlatList(List(f, v))
        } else {
          var fapplst = ListBuffer[LambdaAST]()
          val f1 = flattenFAppList(xs(0))
          val f2 = flattenFAppList(xs(1))
          val f3 = flattenFAppList(xs(2))
          fapplst.append(f1, f2)
          f3 match {
            case FAppFlatList(ys) => fapplst.appendAll(ys)
            case _ => fapplst.append(f3)
          }
          FAppFlatList(fapplst.toList)
        }
      }
      case Fun(a, b) => Fun(a, flattenFAppList(b))
      case Var(a) => Var(a)
    }
  }

  def reduceFAppFlatList(ast: LambdaAST): LambdaAST = {
    ast match {
      case FAppFlatList(xs: List[LambdaAST]) => {
        xs.map(reduceFAppFlatList).reduce(FApp)
      }
      case Fun(a, b) => Fun(a, reduceFAppFlatList(b))
      case Var(a) => Var(a)
    }
  }

  def term: Parser[LambdaAST] = {
    ident | parenExpr | let | LAMBDA ~> fun
  }

  def expr: Parser[LambdaAST] = {
    atLeast2expr | term
  }

  def atLeast2expr: Parser[LambdaAST] = {
    val p1 = term ~ term ~ expr ^^ { case a ~ b ~ c => FAppList(List(a, b, c)) }
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
