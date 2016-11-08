package lexer

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

object LambdaLexer extends RegexParsers {

  val dot    = "."    ^^ { _ => DOT    }
  val lambda = "\\"   ^^ { _ => LAMBDA }
  val open   = "("    ^^ { _ => OPEN   }
  val close  = ")"    ^^ { _ => CLOSE  }
  val let    = "let " ^^ { _ => LET    }
  val equal  = "="    ^^ { _ => EQUAL  }
  val in     = "in "  ^^ { _ => IN     }
  val id     = new Regex("[a-zA-Z_$][a-zA-Z_$0-9]*") ^^ { id => ID(id) }

  val expr = rep1(dot | lambda | open | close | let | equal | in | id)
  val program = phrase(expr)

  def apply(code: String) = {
    parse(program, code) match {
      case NoSuccess(msg, next) => None
      case Success(result, next) => Some(result)
    }
  }
}

