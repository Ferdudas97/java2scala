package java2scala.parser

import java2scala.ast
import java2scala.ast._
import java2scala.keywords.TokenType.{TokenType, _}
import java2scala.keywords._


class Parser private(var tokens: List[Token]) {

  var index = 0
  var currentToken: Token = _
  var className: String = ""

  var counter = 0
  advance()

  def parse(): CompilationUnit = {
    compilationUnit()
  }

  private[parser] def advance(): Unit = if (tokens.nonEmpty) {
    currentToken = tokens.head
    tokens = tokens.tail
  } else NoToken()

  private[parser] def eat(typ: TokenType): Unit = if (currentToken is typ)
    advance()
  else throw new NoSuchElementException(s" token ${currentToken.typ} typ ${typ}")

  private[parser] def peekToken(i: Int = 0) = tokens(i)


  /*
     compilationUnit
     : packageDeclaration? importDeclaration* typeDeclaration* EOF
     ;
      */
  private[parser] def compilationUnit() = {
    val packgeDeclaration = packageDeclaration()
    val importDeclarations = zeroOrMore(_.is(IMPORT), () => importDeclaration())
    val typeDec = typeDeclaration()

    CompilationUnit(packgeDeclaration, ImportDeclarations(importDeclarations), typeDec)
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
    val modifiers = zeroOrMore(_.isInstanceOf[Modifier], classOrIterfaceDelarationModifier)
    val classDec = zeroOrOne(_.is(CLASS), () => classDeclaration(modifiers))
    classDec.getOrElse(interfaceDeclaration(modifiers))
  }

  private[parser] def interfaceDeclaration(modifiers: List[ClassOrInterfaceModifier]): InterfaceDeclaration = {
    eat(INTERFACE)
    val name = identifier()
    val interfaces = zeroOrMore(_.is(COLON), () => eat(COLON), _.is(COMMA), classOrInterfaceType)
    eat(LBRACKET)
    val members = zeroOrMore(!_.is(RBRACKET), interfaceMember)
    InterfaceDeclaration(modifiers, name, members, interfaces)
  }

  private[parser] def interfaceMember(): InterfaceMemberDeclaration = {
    val typee = typeType()
    val name = identifier()
    val param = formalParameters()
    eat(SEMICOLON)
    InterfaceMemberDeclaration(typee, name, param)
  }

  private def classOrIterfaceDelarationModifier() = {
    val modifers = zeroOrMore(_.isInstanceOf[Modifier], modifier)
    ClassOrInterfaceModifier(modifers)
  }

  private[parser] def modifier(): Modifier = {
    val node = currentToken.asInstanceOf[Modifier]
    node match {
      case _: PrivateToken => eat(PRIVATE)
      case _: AbstractToken => eat(ABSTRACT)
      case _: PublicToken => eat(PUBLIC)
      case _: FinalToken => eat(FINAL)
      case _: StaticToken => eat(STATIC)
      case _: OverrideToken => eat(OVERRIDE)
    }
    node
  }


  private def classDeclaration(modifier: List[ClassOrInterfaceModifier]): ClassDeclaration = {
    eat(CLASS)
    val name = identifier()
    className = name.value
    val interfaces = zeroOrMore(_.is(COLON), () => eat(COLON), _.is(COMMA), classOrInterfaceType)
    val body = classBody()
    ClassDeclaration(modifier, name, body, interfaces)
  }

  private[parser] def classBody() = {
    eat(LBRACKET)
    zeroOrMore(_.is(SEMICOLON), () => eat(SEMICOLON))
    val body = zeroOrMore(!_.is(RBRACKET, SEMICOLON), classMemberDeclaration)
    eat(RBRACKET)
    body
  }

  /*

classBodyDeclaration
    : ';'
    | modifier* memberDeclaration
    ;
   */
  private[parser] def classMemberDeclaration(): ClassMemberDeclaration = {
    val modifier = classOrIterfaceDelarationModifier()
    memberDeclaration(modifier)

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
  private[parser] def memberDeclaration(modifier: ClassOrInterfaceModifier): ClassMemberDeclaration = {

    if (currentToken.value == className) constructorDeclaration(modifier)
    else methodOrFieldDeclaration(modifier)
  }

  private[parser] def methodOrFieldDeclaration(modifier: ClassOrInterfaceModifier): ClassMemberDeclaration = {
    val typ = typeType()
    val method = zeroOrOne(_ => peekToken() is LPAREN, () => methodDeclaration(modifier, typ))
    method match {
      case None => fieldDeclaration(modifier, typ)
      case Some(p) => p;
    }
  }

  private[parser] def fieldDeclaration(modifier: ClassOrInterfaceModifier, typeType: TypeType): FieldDeclaration = {
    val declarators = variableDeclarators()
    eat(SEMICOLON)
    FieldDeclaration(modifier, typeType, declarators)
  }

  private[parser] def methodDeclaration(modifier: ClassOrInterfaceModifier, typeType: TypeType): MethodDeclaration = {
    val name = identifier()
    val parameters = formalParameters()
    val body = methodBody()
    MethodDeclaration(modifier, typeType, name, parameters, body)
  }

  private[parser] def methodBody() = {
    val methodBlock = block()
    MethodBody(methodBlock)
  }

  private[parser] def formalParameters() = {
    eat(LPAREN)
    val list = zeroOrOne(!_.is(RPAREN), formalParameterList)
    eat(RPAREN)
    FormalParameters(list.getOrElse(List.empty))
  }

  private[parser] def formalParameterList() = {
    oneOrMore(_.is(COMMA), formalParameter)
  }

  private[parser] def constructorDeclaration(modifier: ClassOrInterfaceModifier) = {
    val name = identifier()
    val parameters = formalParameters()
    val bblock = block()
    ConstructorDeclaration(modifier, name, parameters, bblock)

  }

  private[parser] def formalParameter() = {
    val varModifier = zeroOrOne(_.is(FINAL), () => variableModifier)
    val typ = typeType()
    val id = identifier()
    FormalParameter(varModifier, typ, id)

  }

  private[parser] def methodCall(): MethodCall = {
    val name = identifier()
    val expressions = expressionList()
    MethodCall(name, expressions)
  }

  private[parser] def expressionList(): ExpressionList = {
    eat(LPAREN)
    val expressions = zeroOrMore(!_.is(RPAREN), expression)
    eat(RPAREN)
    ExpressionList(expressions)
  }

  private[parser] def block(): Block = {
    eat(LBRACKET)
    val stmt = zeroOrMore(!_.is(RBRACKET), blockStatement)
    eat(RBRACKET)
    Block(stmt)
  }

  private[parser] def blockStatement(): BlockStatement = {
    val stmt = zeroOrOne(_.is(FINAL, ID, PRIMITIVE) && !peekToken().is(ASSIGN, LPAREN, DOT), localVariableDeclaration)
      .getOrElse(statement())
    BlockStatement(stmt)
  }

  private[parser] def localVariableDeclaration(): LocalVariableDeclaration = {
    val isFinal = zeroOrOne(_.is(FINAL), () => {
      eat(FINAL)
      FinalToken()
    })
    val typ = typeType()
    val declarators = variableDeclarators()
    eat(SEMICOLON)
    LocalVariableDeclaration(isFinal.isDefined, typ, declarators)
  }


  private[parser] def variableDeclarators(): List[VariableDeclarator] = {
    oneOrMore(_.is(COMMA), variableDeclarator)
  }

  private[parser] def variableDeclarator(): VariableDeclarator = {
    val id = variableDeclaratorId()
    val initializer = zeroOrOne(_.is(ASSIGN), () => {
      eat(ASSIGN)
      variableInitializer()
    })
    VariableDeclarator(id, initializer)
  }

  private[parser] def variableInitializer(): VariableInitializer = {
    val node = currentToken

    val initializer = zeroOrOne(_.is(LBRACE), arrayInitializer)

    initializer.getOrElse(VariableInitializerByExpression(expression()))
  }

  private[parser] def arrayInitializer(): ArrayInitializer = {
    eat(LBRACE)
    val values = zeroOrOne(!_.is(RBRACET), () => {
      oneOrMore(_.is(COMMA), () => variableInitializer())
    })
    eat(RBRACET)
    ArrayInitializer(values.getOrElse(List.empty))

  }

  private[parser] def variableDeclaratorId() = {
    val name = identifier()
    val braceNumber = zeroOrMore(_.is(LBRACE), () => {
      eat(LBRACE)
      eat(RBRACET)
      1
    }).size

    VariableDeclaratorId(name, braceNumber)

  }


  def switchStatement(): SwitchStatement = {
    eat(SWITCH)
    val condition = parExpression()
    eat(LBRACKET)
    val groups = zeroOrMore(!_.is(RBRACKET), switchGroup)
    eat(RBRACKET)
    SwitchStatement(condition, groups)
  }

  private[parser] def creator(): Creator = {
    eat(NEW)
    val id = identifier()
    val parameters = expressionList()
    Creator(id, parameters)

  }


  def switchGroup(): SwitchGroup = {
    val label = switchLabel()
    val stmt = blockStatement()
    eat(SEMICOLON)
    SwitchGroup(label, stmt)
  }

  def switchLabel(): SwitchLabel = {
    val exp = zeroOrOne(_.is(CASE), () => {
      eat(CASE)
      val exp = expression()
      eat(COLON)
      exp
    })
    exp match {
      case Some(_) => SwitchLabel(exp)
      case None => eat(DEFAULT); eat(COLON); SwitchLabel(None)
    }
  }

  def whileStatement(): WhileStatement = {
    eat(WHILE)
    val condition = parExpression()
    val stmt = statement()
    WhileStatement(condition, stmt)
  }

  def breakStatement(): BreakStatement = {
    eat(BREAK)
    eat(SEMICOLON)
    BreakStatement()
  }

  def returnStatement(): ReturnStatement = {
    eat(RETURN)
    val exp = expression()
    ReturnStatement(exp)
  }

  def forStatement(): ForStatement = {
    eat(FOR)
    eat(LPAREN)
    val control = forControl()
    eat(RPAREN)
    val blockStmt = block()
    ForStatement(control, blockStmt)
  }

  def forControl(): ForControl = {
    val init = forInit()
    val condition = expression()
    eat(SEMICOLON)
    val update = zeroOrMore(!_.is(RPAREN), expression)
    ForControl(init, condition, update)
  }

  def forInit(): ForInit = {
    val dec = localVariableDeclaration()
    ForInit(dec)
  }

  private[parser] def statement(): Stmt = {
    val node = currentToken
    node match {
      case _: LBracketToken => block()
      case _: IfToken => ifStatement()
      case _: SwitchToken => switchStatement()
      case _: WhileToken => whileStatement()
      case _: ForToken => forStatement()
      case _: BreakToken => breakStatement()
      case _: ReturnToken => returnStatement()
      case s: SemicolonToken => eat(SEMICOLON); s
      case _ => expression()
    }
  }

  private[parser] def ifStatement(): IfStatement = {
    eat(IF)
    val condition = parExpression()
    val stm = statement()
    val elseStmt = zeroOrOne(_.is(ELSE), () => {
      eat(ELSE);
      statement()
    })
    IfStatement(condition, stm, elseStmt)
  }

  private[parser] def parExpression(): ParExp = {
    eat(LPAREN)
    val exp = expression()
    eat(RPAREN)
    ParExp(exp)
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
    val ass = zeroOrOne(_.is(ASSIGN), () => {
      eat(ASSIGN)
      expression2()
    })
    ass match {
      case Some(e) => Assingment(exp, e)
      case None => exp
    }
  }


  private[parser] def expression3(): Exp = {
    currentToken match {
      case _: LiteralToken => primary()
      case l: IdToken => if (peekToken() is LPAREN) methodCall() else identifier()
      case _: LParenToken => parExpression()
      case _: NewToken => creator()
      case _: NotToken => notExp()
      case _ => expression()
    }
  }

  private[parser] def notExp(): NotExp = {
    eat(NOT)
    val exp = expression();
    NotExp(exp)

  }

  private[parser] def expression2(): Exp = {
    val exp3 = expression3()
    val node = currentToken

    val rest = zeroOrOne(_.isInstanceOf[BinOpToken], () => {
      val node = currentToken.asInstanceOf[BinOpToken]
      eat(currentToken.typ)
      BinOp(exp3, node, expression())
    })

    val array = zeroOrOne(_.is(STATIC), () => {
      eat(LBRACE)
      val exp = expression()
      eat(RBRACET)
      ArrayGet(exp3, exp)
    })
    rest.orElse(array).getOrElse(exp3)
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

    val braceNumber = zeroOrMore(_.is(LBRACE),
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
    names = names ::: zeroOrMore(_.is(DOT), () => {
      eat(DOT)
      identifier()
    })
    ClassOrInterfaceType(names)
  }

  private[parser] def primitiveType()

  = {
    val node = currentToken.asInstanceOf[PrimitiveType]
    eat(PRIMITIVE)
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
    names = names ::: zeroOrMore(_.is(DOT), () => {
      eat(DOT)
      identifier()
    })
    QualifiedName(names)
  }

  private[parser] def zeroOrMore[Type](condition: Token => Boolean, parseFunction: () => Type): List[Type]

  = {
    if (condition(currentToken)) parseFunction() :: zeroOrMore(condition, parseFunction) else Nil

  }

  private[parser] def zeroOrMore[Type](startCondition: Token => Boolean, eatFirst: () => Unit, separatorCondition: Token => Boolean, parseFunction: () => Type): List[Type]

  = {
    zeroOrOne(startCondition, eatFirst).map(_ => parseFunction()).map(value => value :: zeroOrMore(separatorCondition, parseFunction))
      .getOrElse(List.empty)
  }

  private[parser] def oneOrMore[Type](condition: Token => Boolean, parseFunction: () => Type): List[Type] = {

    parseFunction() :: zeroOrMore(condition, parseFunction)
  }

  private[parser] def zeroOrOne[Type](condition: Token => Boolean, parseFunction: () => Type): Option[Type]

  = {
    if (condition(currentToken)) Option(parseFunction()) else None
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

object Parser {
  def apply(tokens: List[Token]): Parser = new Parser(tokens)

}
