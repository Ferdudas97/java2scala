package java2scala.parser

import java2scala.ast._
import java2scala.keywords._
import java2scala.lexer.Lexer
import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  val packageDeclaration = PackageDeclaration(QualifiedName(List(IdToken("lol"), IdToken("xd"))));

  val importDeclaration = ImportDeclaration(QualifiedName(List(IdToken("x"), IdToken("asd"), IdToken("xd"))))

  val funDeclaration = MethodDeclaration(ClassOrInterfaceModifier(PublicToken()),
    TypeType(IntToken()),
    IdToken("fun"),
    FormalParameters(List(FormalParameter(Option.empty, TypeType(IntToken()), IdToken("x")))),
    MethodBody(Block(List.empty)))

  val fieldDeclaration = FieldDeclaration(
    ClassOrInterfaceModifier(PrivateToken()),
    TypeType(ClassOrInterfaceType(List(IdToken("String")))),
    List(VariableDeclarator(VariableDeclaratorId(IdToken("test")), Option(VariableInitializerByExpression(StringLiteral('"' + "lol" + '"'))))
    ))

  def sectionParse(text: String, expected: Node, function: Parser => Node) {
    it should s"parse $text" in {


      val tokens = Lexer parse text
      val parser = new Parser(tokens)

      val t = function(parser)
      t shouldBe expected
      parser.tokens.size shouldBe 0
    }
  }

  behave like sectionParse("package lol.xd;", packageDeclaration, p => p.packageDeclaration())

  behave like sectionParse("import x.asd.xd;", importDeclaration, p => p.importDeclaration())

  behave like sectionParse("5 + 6 == 12", BinOp(IntegerLiteral(5), PlusToken(), BinOp(IntegerLiteral(6), EqToken(), IntegerLiteral(12))), p => p.expression())

  behave like sectionParse("public int fun(int x){}",
    funDeclaration,
    p => p.classMemberDeclaration())


  behave like sectionParse(
    """private String test = "lol"; """,
    fieldDeclaration,
    p => p.classMemberDeclaration())

  behave like sectionParse(
    """package lol.xd;
      |
      |import x.asd.xd;
      |
      |public class Ex{
      |
      |private String test = "lol"
      |;
      |public int fun(int x){
      |
      |}
      |}
    """.stripMargin
    ,
    CompilationUnit(packageDeclaration,
      List(importDeclaration),
      ClassDeclaration(List(ClassOrInterfaceModifier(PublicToken())),
        IdToken("Ex"),
        List(fieldDeclaration, funDeclaration))),
    p => p.compilationUnit()
  )
}