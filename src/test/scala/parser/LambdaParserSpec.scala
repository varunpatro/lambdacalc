package parser

import lexer._
import org.scalatest._

import scala.collection.mutable.ArrayBuffer

class LambdaParserSpec extends FlatSpec with Matchers {
  class TestCase(t: List[LambdaToken], e: Some[LambdaAST]) {
    def tokens = t
    def expected = e
  }

  var testCases = ArrayBuffer[TestCase]()
  testCases += new TestCase(List(ID("x")), Some(Var("x")))
  testCases += new TestCase(List(LAMBDA, ID("x"), DOT, ID("x")), Some(Fun("x", Var("x"))))

  "The Lexer" should "lex correctly" in {
    for (testCase <- testCases) {
      LambdaParser(Some(testCase.tokens)) shouldBe testCase.expected
    }
  }
}







