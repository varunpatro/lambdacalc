package parser

import lexer._

import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.parsing.combinator.Parsers

object LambdaParser extends Parsers {
//  val debug = true
    val debug = false
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
    val p = ident | parenExpr | let | LAMBDA ~> fun
    if (debug) log(p)("term") else p
  }

  def expr: Parser[LambdaAST] = {
    val p = atLeast2expr | term
    if (debug) log(p)("expr") else p
  }

  def atLeast2expr: Parser[LambdaAST] = {
    val p1 = term ~ term ~ expr ^^ { case a ~ b ~ c => FAppList(List(a, b, c)) }
    val p2 = term ~ term ^^ { case a ~ b => FAppList(List(a, b)) }
    val p = p1 | p2
    if (debug) log(p)("atLeast2expr") else p
  }

  def ident: Parser[Var] = {
    val p = accept("ident", {case ID(id) => Var(id)})
    if (debug) log(p)("ident") else p
  }

  def parenExpr: Parser[LambdaAST] = {
    val p = OPEN ~> expr <~ CLOSE
    if (debug) log(p)("parenExpr") else p
  }

  def let: Parser[Let] = {
    val p = LET ~ ident ~ EQUAL ~ expr ~ IN ~ expr ^^ {
      case _ ~ n ~ _ ~ f ~ _ ~ v => Let(n.toString, f, v)
    }
    if (debug) log(p)("let") else p
  }

  def fun: Parser[Fun] = {
    val p1 = ident ~ DOT ~ expr ^^ {case arg ~ _ ~ body => Fun(arg.toString, body)}
    val p2 = ident ~ fun ^^ {case arg ~ body => Fun(arg.toString, body)}
    val p = p1 | p2
    if (debug) log(p)("fun") else p
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

  def main(args: Array[String]) {
    var codes: ArrayBuffer[String] = new ArrayBuffer[String]()
    codes += """x y \x.x"""
    codes += """p q"""
    codes += """p q r"""
    codes += """p q r s"""
    codes += """p q r s t"""
    codes += """p q r s t u"""
    codes += """\x.x \x.x"""
    codes += """(\x.x) (\x.x)"""
    codes += """(\x. x) x"""
    codes += """x \x.x y"""
    codes += """\x.x y"""
    codes += """x (\x.x y)"""
    codes += """x"""
    codes += """(x)"""
    codes += """\x.x"""
    codes += """\y.x"""
    codes += """\x y. x"""
    for (code <- codes) {
      println(code)
      val tokens = LambdaLexer(code)
      val ast = LambdaParser(tokens)
      println(ast)
      println()
    }
  }
}
