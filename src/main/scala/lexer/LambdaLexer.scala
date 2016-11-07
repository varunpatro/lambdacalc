package lexer

import scala.util.parsing.combinator.RegexParsers
import scala.util.matching.Regex

object LambdaLexer extends RegexParsers {

  val dot    = "\\.".r  ^^ { _ => DOT    }
  val lambda = "\\\\".r ^^ { _ => LAMBDA }
  val open   = "\\(".r  ^^ { _ => OPEN   }
  val close  = "\\)".r  ^^ { _ => CLOSE  }
  val let    = "let".r  ^^ { _ => LET    }
  val equal  = "=".r    ^^ { _ => EQUAL  }
  val in     = "in"     ^^ { _ => IN     }
  val id     = new Regex("[a-zA-Z_$][a-zA-Z_$0-9]*") ^^ { id => ID(id) }

  val expr = rep1(dot | lambda | open | close | id)
  val program = phrase(expr)

  def apply(code: String) = {
    parse(program, code) match {
      case NoSuccess(msg, next) => None
      case Success(result, next) => Some(result)
    }
  }
}

