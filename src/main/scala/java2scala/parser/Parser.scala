package java2scala.parser

import java2scala.ast._
import java2scala.keywords.TokenType.{TokenType, _}
import java2scala.keywords._

import scala.collection.mutable

class Parser(tokens: List[Token]) {

  var index = 0
  private val queue: mutable.Queue[Token] = mutable.Queue(tokens: _*)
  var currentToken: Token = queue.dequeue()

  def parse(tokens: List[Token]) = {
    compilationUnit()
  }


  def advance() = currentToken = queue.dequeue()

  def eat(typ: TokenType) = if (currentToken is typ)
    advance()
  else throw new NoSuchElementException(s" token ${currentToken.typ} typ ${typ}")

  def peekToken() = queue.front


  /*
     compilationUnit
     : packageDeclaration? importDeclaration* typeDeclaration* EOF
     ;
      */
  def compilationUnit() = {
    val packgeDeclaration = packageDeclaration()
    val importDeclarations = zeroOrMore(() => {
      currentToken is IMPORT
    }, () => importDeclaration())
    val typeDeclaration = typeDeclaration()

    CompilationUnit(packgeDeclaration, importDeclarations, _)
  }

  /*
  packageDeclaration
    : PACKAGE qualifiedName ';'
    ;
   */
  def packageDeclaration() = {
    eat(PACKAGE)
    val name = qualifedName()
    eat(SEMICOLON)
    PackageDeclaration(name)
  }

  /*
  importDeclaration
    : IMPORT  qualifiedName ('.' '*')? ';'
    ;
   */
  def importDeclaration() = {
    eat(IMPORT)
    val name = qualifedName()
    eat(SEMICOLON)
    ImportDeclaration(name)
  }


  private def typeDeclaration(): Unit = {
    val modifiers = zeroOrMore(() => {
      currentToken.isInstanceOf[Modifier]
    },
      classOrIterfaceDelarationModifier)
  }

  private def classOrIterfaceDelarationModifier() = {
    val node = currentToken.asInstanceOf[Modifier]
    node match {
      case _: PrivateToken => eat(PRIVATE)
      case _: AbstractToken => eat(ABSTRACT)
      case _: PublicToken => eat(PUBLIC)
      case _: FinalToken => eat(FINAL)
    }

    ClassOrInterfaceModifier(node)
  }


  private def classDeclaration() = {
    eat(CLASS)
    val name = identifier()
    val body = classBody()
  }

  def classBody() = {
    eat(LBRACKET)
    val body = zeroOrMore(() => !(currentToken is RBRACKET)
      , classBodyDeclaration)
    eat(RBRACKET)
  }

  /*

classBodyDeclaration
    : ';'
    | modifier* memberDeclaration
    ;
   */
  def classBodyDeclaration() = {
    val modifier = classOrIterfaceDelarationModifier()

  }

  /*
  memberDeclaration
    : methodDeclaration
    | fieldDeclaration
    | constructorDeclaration
    | interfaceDeclaration
    | classDeclaration
    | enumDeclaration
    ;
   */
  def memberDeclaration() = {

  }

  /*typeTypeOrVoid
    : typeType
  | VOID
  ;
  */
  def typeOrVoid = {
    val node = currentToken
    node match {
      case _:VoidToken  => eat(VOID)
      case _ => typeType()
    }
  }

  /*
  typeType
    : (classOrInterfaceType | primitiveType) ('[' ']')*
    ;
   */
  def  typeType() = {
    val node = currentToken
    node match {
      case _:PrimitiveType => primitiveType()
      case _ => classOrInterfaceType()
    }

  }

  /*
  classOrInterfaceType
    : IDENTIFIER  ('.' IDENTIFIER )*
    ;
   */

  def classOrInterfaceType() = {
    var names = identifier() :: Nil
    names = names ::: zeroOrMore(() => currentToken is COLON, () => {
      eat(COLON)
      identifier()
    })
    ClassOrInterfaceType(names)
  }

  def primitiveType() = {
    val node = currentToken.asInstanceOf[PrimitiveType]
    node match {
      case _:BooleanToken => eat(BOOLEAN)
      case _ :IntToken => eat(INT)
      case _:ShortToken => eat(SHORT)
      case _:DoubleToken => eat(DOUBLE)
      case _:CharToken => eat(CHAR)
    }
    node
  }
  /*
qualifiedName
  : IDENTIFIER ('.' IDENTIFIER)*
  ;
*/


  private def qualifedName(string: String = ""): QualifiedName = {
    var names = identifier() :: Nil
    names = names ::: zeroOrMore(() => currentToken is COLON, () => {
      eat(COLON)
      identifier()
    })
    QualifiedName(names)
  }

  def zeroOrMore[Type](condition: () => Boolean, parseFunction: () => Type): List[Type] = {
    if (condition()) parseFunction() :: zeroOrMore(condition, parseFunction) else Nil

  }

  def identifier(): IdToken = {
    val node = currentToken
    eat(ID)
    node.asInstanceOf[IdToken]
  }
}
