import parser._

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
      case FApp(f, v) => {
        val ef = eval(f)
        val ev = eval(v)
        ef match {
          case Fun(arg, body) => eval(substitute(body, arg, ev))
          case _ => FApp(ef, ev)
        }
      }
      case Let(key, value, body) => {
        eval(FApp(Fun(key, body), value))
      }
      case Var(_) => ast
      case Fun(_, _) => ast
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

  def main(args: Array[String]) {
    println(apply("""x y z"""))
  }
}