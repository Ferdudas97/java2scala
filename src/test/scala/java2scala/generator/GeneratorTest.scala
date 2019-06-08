package java2scala.generator

import java2scala.lexer.Lexer
import java2scala.parser.Parser
import org.scalatest.{FlatSpec, Matchers}

class GeneratorTest extends FlatSpec with Matchers {

  def test(javaClass: String, expected: String): Unit = {
    val tokens = Lexer.parse(javaClass)

    val parser = Parser(tokens).parse()

    val scala = Visitator.visit(parser)
    RavaCompiler.generate(scala, "/home/radek/IdeaProjects/java2scala/src/test/scala/java2scala/in/TestClass1Sc.scala")
    scala.replaceAll("\\s+", " ") shouldBe expected.replaceAll("\\s+", " ")
  }


  def compile(path: String): Unit = {
    RavaCompiler.files(path)
  }

    behave like compile("/home/radek/IdeaProjects/java2scala/src/test/scala/java2scala/in")
//  behave like test(
//    """package java2scala.in;
//      |
//      |import java.io.File;
//      |
//      |public class TestClass1 {
//      |    private final String text = "text";
//      |    private static int [] [] array;
//      |    public TestClass1(int x) {
//      |     fun(x);
//      |    }
//      |    public int fun(int x) {
//      |        System.out.println(x);
//      |        int y = 5 * (6 + x);
//      |        while (y< 60 and y!=5) {
//      |            y = x + y;
//      |        }
//      |        if (true) {
//      |
//      |        }
//      |        else {
//      |
//      |        }
//      |        boolean f = true;
//      |        f = !f;
//      |        System.out.println(text);
//      |        File file = new File("");
//      |        for(int i =0; i<5; i=i+1){
//      |         System.out.println(i);
//      |        }
//      |        return x;
//      |    }
//      |}
//      |""".stripMargin,
//    """|package java2scala.in
//      |
//      |import java.io.File
//      | class TestClass1  {
//      |private val text : String  = "text"
//      | def fun (x: Int  ) : Int   = {
//      | System .out .println(x )
//      |var y  : Int    = 5 * (6 + x )
//      |while ((y  < 60 || y  != 5)) {
//      |(y  = x  + y )
//      |
//      |}
//      |
//      |if ((true)) {
//      |
//      |}
//      |else {
//      |
//      |}
//      |var f  : Boolean    = true
//      |(f  = !f )
//      |
//      |System .out .println(text )
//      |var file  : File   = new File ("")
//      |return x
//      |
//      |}}
//      |
//  """.stripMargin)



}
