package java2scala.parser

import java2scala.ast._
import java2scala.keywords._
import java2scala.lexer.Lexer
import org.scalatest.{FlatSpec, Matchers}

class ParserSpec extends FlatSpec with Matchers {

  val packageDeclaration = PackageDeclaration(QualifiedName(List(IdToken("lol"), IdToken("xd"))));

  val importDeclaration = ImportDeclaration(QualifiedName(List(IdToken("x"), IdToken("asd"), IdToken("xd"))))

  val constructorDeclaration = ConstructorDeclaration(
    ClassOrInterfaceModifier(
      PrivateToken()),
    IdToken("Ex"),
    FormalParameters(List(
      FormalParameter(None,
        TypeType(ClassOrInterfaceType(
          List(IdToken("String")))),
        IdToken("in")))),
    Block(List(
      BlockStatement(
        Assingment(
          IdToken("test")
          , IdToken("in"))),
      BlockStatement(SemicolonToken()))))

  val forDeclaration = ForStatement(
    ForControl(ForInit(LocalVariableDeclaration(isFinal = false,
      TypeType(IntToken()),
      List(VariableDeclarator(VariableDeclaratorId(IdToken("i")),
        Some(VariableInitializerByExpression(IntegerLiteral(0))))))),
      BinOp(IdToken("i"),
        LtToken(),
        IntegerLiteral(5)),
      List(Assingment(IdToken("i")
        , BinOp(IdToken("i")
          , PlusToken(),
          IntegerLiteral(1))))),
    Block(List()))

  val funDeclaration = MethodDeclaration(ClassOrInterfaceModifier(PublicToken()),
    TypeType(IntToken()),
    IdToken("fun"),
    FormalParameters(List(FormalParameter(Option.empty, TypeType(IntToken()), IdToken("x")))),
    MethodBody(Block(List(BlockStatement(forDeclaration)))))

  val fieldDeclaration = FieldDeclaration(
    ClassOrInterfaceModifier(PrivateToken()),
    TypeType(ClassOrInterfaceType(List(IdToken("String")))),
    List(VariableDeclarator(VariableDeclaratorId(IdToken("test")), Option(VariableInitializerByExpression(StringLiteral('"' + "lol" + '"'))))
    ))


  def sectionParse(text: String, expected: Node, function: Parser => Node) {
    it should s"parse $text" in {
      //      val s= fun();

      val tokens = Lexer parse text
      val parser = new Parser(tokens)

      val t = function(parser)
      t shouldBe expected
      parser.tokens.size shouldBe 0
    }
  }

  behave like sectionParse(
    """
      |public int fun(int x){
      |switch(x) {
      |case 6: "XD";
      |case 8: "X2";
      |}
      |}
    """.stripMargin
    ,funDeclaration,
    p=> p.classMemberDeclaration()
  )
//      behave like sectionParse("package lol.xd;", packageDeclaration, p => p.packageDeclaration())
//
//      behave like sectionParse("import x.asd.xd;", importDeclaration, p => p.importDeclaration())
//
//      behave like sectionParse("5 + 6 == 12", BinOp(IntegerLiteral(5), PlusToken(), BinOp(IntegerLiteral(6), EqToken(), IntegerLiteral(12))), p => p.expression())
//
//    behave like sectionParse("public int fun(int x){" +
//      "for(int i =0; i<5; i=i+1){}" +
//      "}",
//      funDeclaration,
//      p => p.classMemberDeclaration())
//
//
//
//      behave like sectionParse(
//        """private String test = "lol"; """,
//        fieldDeclaration,
//        p => p.classMemberDeclaration())
//
//  behave like sectionParse(
//    """package lol.xd;
//      |
//      |import x.asd.xd;
//      |
//      |public class Ex{
//      |
//      |private String test = "lol";
//      |private Ex(String in) {
//      |test = in;
//      |}
//      |public int fun(int x){
//      |for(int i =0; i<5; i=i+1){}
//      |}
//      |}
//    """.stripMargin
//    ,
//    CompilationUnit(packageDeclaration,
//      ImportDeclarations(List(importDeclaration)),
//      ClassDeclaration(List(ClassOrInterfaceModifier(PublicToken())),
//        IdToken("Ex"),
//        List(fieldDeclaration, constructorDeclaration, funDeclaration))),
//    p => p.compilationUnit()
//  )
//
//      behave like sectionParse(
//        """switch(x) {
//          |case 5 : 6
//          |}
//        """.stripMargin,
//        SwitchStatement(ParExp(IdToken("x")),
//          List(SwitchGroup(SwitchLabel(Option(IntegerLiteral(5))), BlockStatement(IntegerLiteral(6))))),
//        p => p.switchStatement())
//
//
//        behave like sectionParse(
//          "for(int i =0; i<5; i=i+1){}"
//          , forDeclaration
//          , p => p.forStatement()
//        )



//    def fun(): String= {
//      Visitator.visit(CompilationUnit(packageDeclaration,
//        List(importDeclaration),
//        ClassDeclaration(List(ClassOrInterfaceModifier(PublicToken())),
//          IdToken("Ex"),
//          List(fieldDeclaration, funDeclaration))))
//    }
}