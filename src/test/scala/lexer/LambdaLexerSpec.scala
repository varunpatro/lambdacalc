package lexer

import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class LambdaLexerSpec extends FlatSpec with Matchers {
  class TestCase(c: String, e: Option[List[LambdaToken]]) {
    def code = c
    def expected = e
  }

  var testCases = ArrayBuffer[TestCase]()
  testCases += new TestCase("""x""", Some(List(ID("x"))))
  testCases += new TestCase("""\x.x""", Some(List(LAMBDA, ID("x"), DOT, ID("x"))))
  testCases += new TestCase("""\f.\x.(f x)""", Some(List(LAMBDA, ID("f"), DOT, LAMBDA, ID("x"), DOT, OPEN, ID("f"), ID("x"), CLOSE)))
  testCases += new TestCase("""\f.\g.\x.(f (g x))""", Some(List(LAMBDA, ID("f"), DOT, LAMBDA, ID("g"), DOT, LAMBDA, ID("x"), DOT, OPEN, ID("f"), OPEN, ID("g"), ID("x"), CLOSE, CLOSE)))
  testCases += new TestCase("""\f.\x.\y.((f x) y)""", Some(List(LAMBDA, ID("f"), DOT, LAMBDA, ID("x"), DOT, LAMBDA, ID("y"), DOT, OPEN, OPEN, ID("f"), ID("x"), CLOSE, ID("y"), CLOSE)))
  testCases += new TestCase("""\x.\y.(\z.y x)""", Some(List(LAMBDA, ID("x"), DOT, LAMBDA, ID("y"), DOT, OPEN, LAMBDA, ID("z"), DOT, ID("y"), ID("x"), CLOSE)))
  testCases += new TestCase("""\x.\y.(y x)""", Some(List(LAMBDA, ID("x"), DOT, LAMBDA, ID("y"), DOT, OPEN, ID("y"), ID("x"), CLOSE)) )
  testCases += new TestCase("""(\x.(x x) \x.(x x))""", Some(List(OPEN, LAMBDA, ID("x"), DOT, OPEN, ID("x"), ID("x"), CLOSE, LAMBDA, ID("x"), DOT, OPEN, ID("x"), ID("x"), CLOSE, CLOSE)))

  "The Lexer" should "lex correctly" in {
    for (testCase <- testCases) {
      LambdaLexer(testCase.code) shouldBe testCase.expected
    }
  }
}






