package java2scala.parser

import java2scala.ast._
import java2scala.keywords.{EqToken, IdToken, PlusToken}
import java2scala.lexer.Lexer
import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {


  def sectionParse(text: String, expected: Node, function: Parser => Node) {
    it should s"parse $text" in {


      val tokens = new Lexer().parse(text)
      val parser = new Parser(tokens)

      val t = function(parser)
      t shouldBe expected
    }
  }

  behave like sectionParse("package lol.xd;", PackageDeclaration(QualifiedName(List(IdToken("lol"), IdToken("xd")))), p => p.packageDeclaration())

  behave like sectionParse("import x.asd.xd;", ImportDeclaration(QualifiedName(List(IdToken("x"), IdToken("asd"), IdToken("xd")))), p => p.importDeclaration())

  behave like sectionParse("5 + 6 == 12", BinOp(IntegerLiteral(5), PlusToken(), BinOp(IntegerLiteral(6), EqToken(), IntegerLiteral(12))), p => p.expression())


}
