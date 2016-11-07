package interpreter

import compiler.LambdaCompiler
import evaluator.LambdaEvaluator

object LambdaInterpreter {
  def main(args: Array[String]): Unit = {
    def readExpr = {
      print("Lambda Expression: ")
      readLine()
    }

    var line = readExpr
    while(line != "exit") {
      val ast = LambdaCompiler(line)
      ast match {
        case Some(v) => println("Parse Result: " + v)
        case None => println("Parse Result: " + None)
      }
      val res = LambdaEvaluator(line)
      res match {
        case Some(v) => println("Eval Result: " + v)
        case None => println("Eval Result: " + None)
      }
      println()
      line = readExpr
    }
  }
}