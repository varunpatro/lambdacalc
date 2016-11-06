import parser._

import scala.util.Try

object LambdaEvaluator {
  def substitute(ast: LambdaAST, key: String, value: LambdaAST): LambdaAST = {
    ast match {
      case Var(x) => {
        if (x == key) value
        else ast
      }
      case FApp(f, v) => {
        val f_sub = substitute(f, key, value)
        val v_sub = substitute(v, key, value)
        FApp(f_sub, v_sub)
      }
      case Fun(arg, body) => {
        if (arg == key) ast
        else Fun(arg, substitute(body, key, value))
      }
    }
  }

  def eval(ast: LambdaAST): LambdaAST = {
    ast match {
      case Fun(_, _) => ast
      case FApp(f, v) => {
        f match {
          case Fun(arg, body) => eval(substitute(body, arg, eval(v)))
          case _ => eval(FApp(eval(f), v))
        }
      }
      case Var(x) => throw new Exception("Undefined variable: " + x)
      case _ => throw new Exception("Invalid expression.")
    }
  }

  def apply(code: String): Option[LambdaAST] = {
    try {
      LambdaCompiler(code).collect { case a => eval(a) }
    } catch {
      case _ => None
    }
  }
}