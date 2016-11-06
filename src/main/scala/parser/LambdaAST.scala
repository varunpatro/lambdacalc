package parser

sealed trait LambdaAST
  case class Var(name: String) extends LambdaAST {
    override def toString = name
  }
  case class Fun(arg: String, body: LambdaAST) extends LambdaAST {
    override def toString = "\\" ++ arg ++ "." ++ body.toString
  }
  case class FApp(f: LambdaAST, v: LambdaAST) extends LambdaAST {
    override def toString = "(" ++ f.toString ++ " " ++ v.toString  ++ ")"
  }
  case class Let(n: String,t1: LambdaAST, t2: LambdaAST) extends LambdaAST {
    override def toString = "(let " ++ n.toString ++ "=" ++ t1.toString ++ " in " ++ t2.toString ++ ")"
  }
  case class FAppFlatList(f: List[LambdaAST]) extends LambdaAST {
   override def toString = "{" + (f mkString ",") + "}"
  }
  case class FAppList(f: List[LambdaAST]) extends LambdaAST {
   override def toString = "[" + (f mkString ",") + "]"
  }
