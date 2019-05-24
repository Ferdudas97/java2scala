package java2scala.generator

import java2scala.lexer.Lexer
import java2scala.parser.Parser
import org.scalatest.{FlatSpec, Matchers}

class GeneratorTest extends FlatSpec with Matchers{

  def test(javaClass: String): Unit = {
     val tokens =  Lexer.parse(javaClass)

    val parser = new Parser(tokens).parse(tokens)

    val scala  = Visitator.visit(parser)
    ""
  }


  behave like test("""package javascala.ast;
                     |
                     |public class ad {
                     |
                     |private int x = 5;
                     |
                     |public int fun(int x) {
                     |switch(x) {
                     |case 6: "XD";
                     |case 8: "X2";
                     |}
                     |}
                     |""".stripMargin)

}
