package java2scala

import java2scala.keywords.{NoToken, PrivateToken, Token}
import java2scala.lexer.Lexer
import java2scala.parser.Parser
import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers{


  "A parser" should "parse import section" in {
    val text =
      """package lol.xd;
        |import a.x.d;
        |import x.sg;
      """.stripMargin
    val tokens = new Lexer().parse(text)
    val parser = new Parser(tokens)

     val t =parser.parse(tokens)


  }

}
