package compiler

import org.scalatest.{FlatSpec, Matchers}
import parser._

import scala.collection.mutable.ArrayBuffer


class LambdaCompilerSpec extends FlatSpec with Matchers {
  class TestCase(c: String, e: Option[LambdaAST]) {
    def code = c
    def expected = e
  }

  var testCases = ArrayBuffer[TestCase]()

//  identifiers
  testCases += new TestCase("""x""", Some(Var("x")))
  testCases += new TestCase("""(x)""", Some(Var("x")))

//  functions
  testCases += new TestCase("""\x.x""", Some(Fun("x", Var("x"))))
  testCases += new TestCase("""\y.x""", Some(Fun("y", Var("x"))))
  testCases += new TestCase("""\x.x y""", Some(Fun("x", FApp(Var("x"), Var("y")))))
  testCases += new TestCase("""\x y. x""", Some(Fun("x", Fun("y", Var("x")))))
  testCases += new TestCase("""\f.\x.(f x)""", Some(Fun("f", Fun("x", FApp(Var("f"), Var("x"))))))
  testCases += new TestCase("""\f.\g.\x.(f (g x))""", Some(Fun("f", Fun("g", Fun("x", FApp(Var("f"), FApp(Var("g"), Var("x"))))))))
  testCases += new TestCase("""\f.\x.\y.((f x) y)""", Some(Fun("f", Fun("x", Fun("y", FApp(FApp(Var("f"), Var("x")), Var("y")))))))
  testCases += new TestCase("""\x.\y.(\z.y x)""", Some(Fun("x", Fun("y", Fun("z", FApp(Var("y"), Var("x")))))))
  testCases += new TestCase("""\x.\y.(y x)""", Some(Fun("x", Fun("y", FApp(Var("y"), Var("x"))))))

//  applications
  testCases += new TestCase("""(\x.x) (\x.x)""", Some(FApp(Fun("x", Var("x")), Fun("x", Var("x")))))
  testCases += new TestCase("""(\x.(x x)) (\x.(x x))""", Some(FApp(Fun("x", FApp(Var("x"), Var("x"))), Fun("x", FApp(Var("x"), Var("x"))))))
  testCases += new TestCase("""x y \x.x""", Some(FApp(FApp(Var("x"), Var("y")), Fun("x", Var("x")))))
  testCases += new TestCase("""\x.x \x.x""", Some(Fun("x", FApp(Var("x"), Fun("x", Var("x"))))))
  testCases += new TestCase("""(\x. x) x""", Some(FApp(Fun("x", Var("x")), Var("x"))))
  testCases += new TestCase("""x \x.x y""", Some(FApp(Var("x"), Fun("x", FApp(Var("x"), Var("y"))))))
  testCases += new TestCase("""x (\x.x y)""", Some(FApp(Var("x"), Fun("x", FApp(Var("x"), Var("y"))))))
  testCases += new TestCase("""p q""", Some(FApp(Var("p"), Var("q"))))
  testCases += new TestCase("""p q r""", Some(FApp(FApp(Var("p"), Var("q")), Var("r"))))
  testCases += new TestCase("""p q r s""", Some(FApp(FApp(FApp(Var("p"), Var("q")), Var("r")), Var("s"))))
  testCases += new TestCase("""p q r s t""", Some(FApp(FApp(FApp(FApp(Var("p"), Var("q")), Var("r")), Var("s")), Var("t"))))
  testCases += new TestCase("""p q r s t u""", Some(FApp(FApp(FApp(FApp(FApp(Var("p"), Var("q")), Var("r")), Var("s")), Var("t")), Var("u"))))


  "The Compiler" should "compile correctly" in {
    for (testCase <- testCases) {
      val ast = LambdaCompiler(testCase.code)
      ast shouldBe testCase.expected
    }
  }
}


