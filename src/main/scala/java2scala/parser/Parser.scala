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


  private[parser] def advance() = currentToken = queue.dequeue()

  private[parser] def eat(typ: TokenType) = if (currentToken is typ)
    advance()
  else throw new NoSuchElementException(s" token ${currentToken.typ} typ ${typ}")

  private[parser] def peekToken() = queue.front


  /*
     compilationUnit
     : packageDeclaration? importDeclaration* typeDeclaration* EOF
     ;
      */
  private[parser] def compilationUnit() = {
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
  private[parser] def packageDeclaration() = {
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
  private[parser] def importDeclaration() = {
    eat(IMPORT)
    val name = qualifedName()
    eat(SEMICOLON)
    ImportDeclaration(name)
  }


  private[parser] def typeDeclaration(): Unit = {
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

  private[parser] def classBody() = {
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
  private[parser] def classBodyDeclaration() = {
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
  private[parser] def memberDeclaration() = {

    currentToken match {
      case _: IdToken => ConstructorDeclaration()
      case _ => methodOrFieldDeclaration()
    }
    val tov = typeOrVoid
  }

  private[parser] def methodOrFieldDeclaration() = {
    val name = identifier()

  }


  private[parser] def formalParameters() = {
    eat(LPAREN)
    val list = formalParameterList()
    eat(RPAREN)
  }

  private[parser] def formalParameterList() = {

  }

  private[parser] def ConstructorDeclaration() = {
    val name = identifier()

  }


  /*typeTypeOrVoid
    : typeType
  | VOID
  ;
  */
  private[parser] def typeOrVoid = {
    val node = currentToken
    val typ: Either[VoidToken, TypeType] = node match {
      case v: VoidToken => eat(VOID); Left(v)
      case _ => Right(typeType())
    }
    TypeOrVoid(typ)

  }

  /*
  typeType
    : (classOrInterfaceType | primitiveType) ('[' ']')*
    ;
   */
  private[parser] def typeType() = {
    val node = currentToken
    val typ = node match {
      case _: PrimitiveType => primitiveType()
      case _ => classOrInterfaceType()
    }

    val braceNumber = zeroOrMore(() => currentToken is LBRACE,
      () => {
        eat(LBRACE)
        eat(RBRACET)
        1
      }
    ).size

    TypeType(typ, braceNumber)

  }

  /*
  classOrInterfaceType
    : IDENTIFIER  ('.' IDENTIFIER )*
    ;
   */

  private[parser] def classOrInterfaceType() = {
    var names = identifier() :: Nil
    names = names ::: zeroOrMore(() => currentToken is COLON, () => {
      eat(COLON)
      identifier()
    })
    ClassOrInterfaceType(names)
  }

  private[parser] def primitiveType() = {
    val node = currentToken.asInstanceOf[PrimitiveType]
    node match {
      case _: BooleanToken => eat(BOOLEAN)
      case _: IntToken => eat(INT)
      case _: ShortToken => eat(SHORT)
      case _: DoubleToken => eat(DOUBLE)
      case _: CharToken => eat(CHAR)
    }
    node
  }

  /*
qualifiedName
  : IDENTIFIER ('.' IDENTIFIER)*
  ;
*/


  private[parser] def qualifedName(string: String = ""): QualifiedName = {
    var names = identifier() :: Nil
    names = names ::: zeroOrMore(() => currentToken is COLON, () => {
      eat(COLON)
      identifier()
    })
    QualifiedName(names)
  }

  private[parser] def zeroOrMore[Type](condition: () => Boolean, parseFunction: () => Type): List[Type] = {
    if (condition()) parseFunction() :: zeroOrMore(condition, parseFunction) else Nil

  }

  private[parser] def identifier(): IdToken = {
    val node = currentToken
    eat(ID)
    node.asInstanceOf[IdToken]
  }
}
