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


  private[parser] def advance() = currentToken = if (queue.nonEmpty) queue.dequeue() else NoToken()

  private[parser] def eat(typ: TokenType) = if (currentToken is typ)
    advance()
  else throw new NoSuchElementException(s" token ${currentToken.typ} typ ${typ}")

  private[parser] def peekToken() = queue.front

  private def is(types: TokenType*): Boolean = types.exists(is)

  private def is(typ: TokenType): Boolean = currentToken is typ


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
    val typeDec = typeDeclaration()

    CompilationUnit(packgeDeclaration, importDeclarations, typeDec)
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


  private[parser] def typeDeclaration(): TypeDeclaration = {
    val modifiers = zeroOrMore(() => {
      currentToken.isInstanceOf[Modifier]
    },
      classOrIterfaceDelarationModifier)
    zeroOrOne(() => is(CLASS), classDeclaration)
    null
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


  private def classDeclaration(): ClassDeclaration = {
    eat(CLASS)
    val name = identifier()
    val body = classBody()
    null
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
    val member = memberDeclaration()
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
  }

  private[parser] def methodOrFieldDeclaration() = {
    val typ = typeType()
    val name = identifier()
    zeroOrOne(() => is(LPAREN), methodDeclaration)
  }


  private[parser] def methodDeclaration() = {
    val parameters = formalParameters()
    val body = methodBody()
  }

  private[parser] def methodBody() = {
    eat(LBRACKET)

    eat(RBRACKET)
  }

  private[parser] def formalParameters() = {
    eat(LPAREN)
    val list = formalParameterList()
    eat(RPAREN)
    FormalParameters(list)
  }

  private[parser] def formalParameterList() = {
    val firstArgument = zeroOrOne(() => is(FINAL, ID), formalParameter)
    firstArgument.map(a => a :: zeroOrMore(() => is(COLON), formalParameter))
      .getOrElse(Nil)
  }

  private[parser] def ConstructorDeclaration() = {
    val name = identifier()

  }

  private[parser] def formalParameter() = {
    val varModifier = zeroOrOne(() => is(FINAL), () => variableModifier)
    val typ = typeType()
    val id = identifier()
    FormalParameter(varModifier, typ, id)

  }

  private[parser] def block(): Stmt = {
    eat(LBRACKET)
    val stmt = blockStatement()
    eat(RBRACKET)
    stmt
  }

  private[parser] def blockStatement(): Stmt = {
    block()
  }

  def switchStatement(): Stmt = {
    eat(SWITCH)
    val condition = parExpression()
    eat(LBRACKET)
    //switchBlockStatementGroups
    eat(RBRACKET)
    null
  }

  def whileStatement() = {
    eat(WHILE)
    val condition = parExpression()
    val stmt = statement()
    WhileStatement(condition, stmt)
  }

  def breakStatement(): Stmt = {
    eat(BREAK)
    eat(SEMICOLON)
    BreakStatement()
  }

  def returnStatement(): Stmt = {
    eat(RETURN)
    val exp = expression()
    ReturnStatement(exp)
  }

  private[parser] def statement(): Stmt = {
    val node = currentToken
    node match {
      case _: LBracketToken => block()
      case _: IfToken => ifStatement()
      case _: SwitchToken => switchStatement()
      case _: WhileToken => whileStatement()
      //      case _: ForToken => forStatement()
      case _: BreakToken => breakStatement()
      case _: ReturnToken => returnStatement()
    }
  }

  private[parser] def ifStatement(): IfStatement = {
    eat(IF)
    val condition = parExpression()
    val stm = statement()
    val elseStmt = zeroOrOne(() => is(ELSE), () => {
      eat(ELSE);
      statement()
    })
    IfStatement(condition, stm, elseStmt)
  }

  private[parser] def parExpression(): Exp = {
    eat(LPAREN)
    val exp = expression()
    eat(RPAREN)
    exp
  }

  //  private[parser]  def expression(): Exp = {
  //    val node = currentToken;
  //    node match {
  //      case _:LParenToken=> parExpression()
  //      case _:NewToken=> ;
  //    }
  //
  //  }
  private[parser] def expression(): Exp = {
    val node = currentToken
    val exp = expression2()
    zeroOrOne(() => is(ASSIGN), () => {
      eat(ASSIGN)
      expression2()
    }) match {
      case Some(e) => Assingment(exp, e)
      case None => exp
    }
  }


  private[parser] def expression3(): Exp = {
    currentToken match {
      case _:LiteralToken =>primary()
      case _  => expression()
    }
  }

  private[parser] def expression2(): Exp = {
    val exp3 = expression3()
    val node = currentToken

    val rest = zeroOrOne(() => currentToken.isInstanceOf[BinOpToken], () => {
      val node = currentToken.asInstanceOf[BinOpToken]
      eat(currentToken.typ)
      BinOp(exp3, node, expression())
    })

    rest.getOrElse(exp3)
  }

  private[parser] def primary(): Exp = {
    val node = currentToken;
    node match {
      case l: LParenToken => parExpression()
      case _ => literal()
    }
  }

  private[parser] def literal(): Literal = {
    val node = currentToken
    eat(LITERAL)
    node.asInstanceOf[LiteralToken] match {
      case _: FalseToken => BooleanLiteral(false)
      case _: TrueToken => BooleanLiteral(true)
      case t: NumberToken => IntegerLiteral(t.value.toInt)
      case t: FloatNumberToken => FloatLiteral(t.value.toFloat)
      case t: StringToken => StringLiteral(t.value)
      case _: NullToken => NullLiteral()
    }
  }

  /*typeTypeOrVoid
    : typeType
  | VOID
  ;
  */
  private[parser] def typeOrVoid

  = {
    val node = currentToken
    val typ: Either[VoidToken, TypeType] = node match {
      case v: VoidToken => eat(VOID);
        Left(v)
      case _ => Right(typeType())
    }
    TypeOrVoid(typ)

  }

  /*
  typeType
    : (classOrInterfaceType | primitiveType) ('[' ']')*
    ;
   */
  private[parser] def typeType()

  = {
    val node = currentToken
    val typ = node match {
      case _: PrimitiveType => primitiveType()
      case _ => classOrInterfaceType()
    }

    val braceNumber = zeroOrMore(() => is(LBRACE),
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

  private[parser] def classOrInterfaceType()

  = {
    var names = identifier() :: Nil
    names = names ::: zeroOrMore(() => is(COLON), () => {
      eat(COLON)
      identifier()
    })
    ClassOrInterfaceType(names)
  }

  private[parser] def primitiveType()

  = {
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


  private[parser] def qualifedName(string: String = ""): QualifiedName

  = {
    var names = identifier() :: Nil
    names = names ::: zeroOrMore(() => is(COLON), () => {
      eat(COLON)
      identifier()
    })
    QualifiedName(names)
  }

  private[parser] def zeroOrMore[Type](condition: () => Boolean, parseFunction: () => Type): List[Type]

  = {
    if (condition()) parseFunction() :: zeroOrMore(condition, parseFunction) else Nil

  }

  private[parser] def zeroOrOne[Type](condition: () => Boolean, parseFunction: () => Type): Option[Type]

  = {
    if (condition()) Option(parseFunction()) else None
  }

  private[parser] def identifier(): IdToken

  = {
    val node = currentToken
    eat(ID)
    node.asInstanceOf[IdToken]
  }


  private[parser] def variableModifier: VariableModifier

  = {
    val node = currentToken
    eat(FINAL)
    node.asInstanceOf[FinalToken]
  }
}
