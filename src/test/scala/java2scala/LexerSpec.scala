package java2scala

import java2scala.keywords._
import java2scala.lexer.Lexer
import org.scalatest.{BeforeAndAfterEach, FlatSpec, FunSuite, Matchers}

class LexerSpec extends FlatSpec with Matchers with BeforeAndAfterEach{
  var lexer: Lexer = new Lexer()

  "A lexer" should "tokenize ex1" in {
    val ex1 = "><!==="
    val tokens = lexer parse ex1
    val res = tokens.map(t => t.value) mkString;
    ex1 shouldBe res
  }

  it should "tokenize ex2" in {
    val ex1 = "private short lol = 2;"
    val tokens = lexer parse ex1
    val expected = List(PrivateToken(1), ShortToken(1), IdToken("lol", 1), AssignToken(1), NumberToken("2", 1), SemicolonToken(1))
    tokens shouldBe expected
  }

  it should "tokenize ex3" in {
    val ex =
      """public abstract class Ex {
        |int x = 5;
        |public void set(int n) {
        |x = n;
        |}
        |}""".stripMargin
    val expected = List(PublicToken(1), AbstractToken(1), ClassToken(1), IdToken("Ex", 1), LBracketToken(1),
      IntToken(2), IdToken("x", 2), AssignToken(2), NumberToken("5", 2), SemicolonToken(2),
      PublicToken(3), VoidToken(3), IdToken("set", 3), LParenToken(3), IntToken(3), IdToken("n", 3), RParenToken(3), LBracketToken(3),
      IdToken("x", 4), AssignToken(4), IdToken("n", 4), SemicolonToken(4),
      RBracketToken(5),
      RBracketToken(6)
    )
    val tokens = lexer parse ex
    tokens shouldBe expected
  }
  it should "tokenize ex4" in {
    val ex1 = "\"string\""
    val tokens = lexer parse ex1
    val expected = List(StringToken("\"string\"",1))
    tokens shouldBe expected
  }

  override protected def beforeEach(): Unit = lexer = new Lexer()
}
