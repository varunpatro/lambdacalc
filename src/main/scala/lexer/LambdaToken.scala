package lexer

sealed trait LambdaToken
case object DOT extends LambdaToken {
  override def toString = "DOT"
}
case object LAMBDA extends LambdaToken {
  override def toString = "LAMBDA"
}
case object OPEN extends LambdaToken {
  override def toString = "OPEN"
}
case object CLOSE extends LambdaToken {
  override def toString = "CLOSE"
}
case object LET extends LambdaToken {
  override def toString = "LET"
}
case object EQUAL extends LambdaToken {
  override def toString = "EQUAL"
}
case object IN extends LambdaToken {
  override def toString = "IN"
}
case object END extends LambdaToken {
  override def toString = "END"
}
case class ID(s: String) extends LambdaToken {
  override def toString = "ID(" + s + ")"
}
