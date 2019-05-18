package java2scala

import java2scala.keywords._
import java2scala.lexer.Lexer
import org.scalatest.{BeforeAndAfterEach, FlatSpec, FunSuite, Matchers}

import scala.xml.dtd.Tokens

class LexerSpec extends FlatSpec with Matchers {

  def tokenize(example: String, expected: List[Token]): Unit = {

    val acutal = Lexer.parse(example)
    acutal shouldBe expected
  }

  behave like tokenize("><!===", List(GtToken(), LtToken(), NeqToken(), EqToken()))

  behave like tokenize("private short lol = 2;",
    List(PrivateToken(), ShortToken(), IdToken("lol"), AssignToken(), NumberToken("2"), SemicolonToken()))

  behave like tokenize(
    """public abstract class Ex {
      |int x = 5;
      |public void set(int n) {
      |x = n;
      |}
      |}""".stripMargin,
    List(PublicToken(), AbstractToken(), ClassToken(), IdToken("Ex"), LBracketToken(),
      IntToken(), IdToken("x"), AssignToken(), NumberToken("5"), SemicolonToken(),
      PublicToken(), VoidToken(), IdToken("set"), LParenToken(), IntToken(), IdToken("n"), RParenToken(), LBracketToken(),
      IdToken("x"), AssignToken(), IdToken("n"), SemicolonToken(),
      RBracketToken(),
      RBracketToken()
    ))

  behave like tokenize("\"string\"", List(StringToken("\"string\"")))


  behave like tokenize(
    """
      |
      |
    """.stripMargin,
    List.empty)
}
