import org.scalatest.{FlatSpec, Matchers}
import parser._

import scala.collection.mutable.ArrayBuffer


class LambdaEvaluatorSpec extends FlatSpec with Matchers {
  class TestCase(c: String, e: Option[LambdaAST]) {
    def code = c
    def expected = e
  }

  var testCases = ArrayBuffer[TestCase]()

//  functions
  testCases += new TestCase("""\x.x""", Some(Fun("x", Var("x"))))
  testCases += new TestCase("""\x.x x""", Some(Fun("x", FApp(Var("x"), Var("x")))))
  testCases += new TestCase("""\f.\x.(x f)""", Some(Fun("f", Fun("x", FApp(Var("x"), Var("f"))))))
  testCases += new TestCase("""\f x.x f""", Some(Fun("f", Fun("x", FApp(Var("x"), Var("f"))))))
  testCases += new TestCase("""\f.\g.\x.(f (g x))""", Some(Fun("f", Fun("g", Fun("x", FApp(Var("f"), FApp(Var("g"), Var("x"))))))))
  testCases += new TestCase("""\f g x.(f (g x))""", Some(Fun("f", Fun("g", Fun("x", FApp(Var("f"), FApp(Var("g"), Var("x"))))))))
  testCases += new TestCase("""\f.\x.\y.((f x) y)""", Some(Fun("f", Fun("x", Fun("y", FApp(FApp(Var("f"), Var("x")), Var("y")))))))
  testCases += new TestCase("""\x.\y.(\z.y x)""", Some(Fun("x", Fun("y", Fun("z", FApp(Var("y"), Var("x")))))))
  testCases += new TestCase("""\x.\y.(y x)""", Some(Fun("x", Fun("y", FApp(Var("y"), Var("x"))))))

//  applications
  testCases += new TestCase("""(\x.x) (\x.x)""", Some(Fun("x", Var("x"))))
  testCases += new TestCase("""(((\x y z.y z) \a.a) \b.b) \c.c""", Some(Fun("c", Var("c"))))

//  failures
  testCases += new TestCase("""x""", None)
  testCases += new TestCase("""p q""", None)
  testCases += new TestCase("""p q r""", None)
  testCases += new TestCase("""p q r s""", None)
  testCases += new TestCase("""x y \x.x""", None)

//  non-terminating
//  testCases += new TestCase("""(\x.(x x)) (\x.(x x))""", Some(FApp(Fun("x", FApp(Var("x"), Var("x"))), Fun("x", FApp(Var("x"), Var("x")))))) // Infinite Loop!


  "The Evaluator" should "evaluate correctly" in {
    for (testCase <- testCases) {
      val ast = LambdaEvaluator(testCase.code)
      ast shouldBe testCase.expected
    }
  }
}


