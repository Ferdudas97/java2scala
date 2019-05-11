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
    val expected = List(PrivateToken(), ShortToken(), IdToken("lol"), AssignToken(), NumberToken("2"), SemicolonToken())
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
    val expected = List(PublicToken(), AbstractToken(), ClassToken(), IdToken("Ex"), LBracketToken(),
      IntToken(), IdToken("x"), AssignToken(), NumberToken("5"), SemicolonToken(),
      PublicToken(), VoidToken(), IdToken("set"), LParenToken(), IntToken(), IdToken("n"), RParenToken(), LBracketToken(),
      IdToken("x"), AssignToken(), IdToken("n"), SemicolonToken(),
      RBracketToken(),
      RBracketToken()
    )
    val tokens = lexer parse ex
    tokens shouldBe expected
  }
  it should "tokenize ex4" in {
    val ex1 = "\"string\""
    val tokens = lexer parse ex1
    val expected = List(StringToken("\"string\""))
    tokens shouldBe expected
  }

  override protected def beforeEach(): Unit = lexer = new Lexer()
}
