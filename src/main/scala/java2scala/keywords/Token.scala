package java2scala.keywords

import java2scala.ast.{Exp, Stmt, Type, TypeType}
import java2scala.keywords.TokenType.TokenType


sealed abstract class Token(val value: String, val typ: TokenType) {
  def is(tokenType: TokenType): Boolean = this.typ == tokenType
  def is(types: TokenType*): Boolean = types.exists(is)
}

sealed abstract class Modifier(override val value: String, override val typ: TokenType) extends Token(value, typ)

sealed abstract class PrimitiveType(override val value: String, override val typ: TokenType) extends Token(value, typ) with Type

sealed trait VariableModifier

sealed abstract class BinOpToken(override val value: String, override val typ: TokenType) extends Token(value, typ)

case class ClassToken() extends Token("class", TokenType.CLASS)

case class ImplementsToken() extends Token("implements", TokenType.IMPLEMENTS)

case class InterfaceToken() extends Token("interface", TokenType.INTERFACE)

case class ExtendsToken() extends Token("extends", TokenType.EXTENDS)

case class OverrideToken() extends Modifier("override", TokenType.OVERRIDE)

case class AbstractToken() extends Modifier("abstract", TokenType.ABSTRACT)

case class ContinueToken() extends Token("continue", TokenType.CONTINUE)

case class ForToken() extends Token("for", TokenType.FOR)

case class DoToken() extends Token("do", TokenType.DO)

case class NewToken() extends Token("new", TokenType.NEW)

case class SwitchToken() extends Token("switch", TokenType.SWITCH)

case class PackageToken() extends Token("package", TokenType.PACKAGE)

case class ImportToken() extends Token("import", TokenType.IMPORT)

case class BooleanToken() extends PrimitiveType("boolean", TokenType.PRIMITIVE)

case class PrivateToken() extends Modifier("private", TokenType.PRIVATE)

case class ThisToken() extends Token("this", TokenType.THIS)

case class BreakToken() extends Token("break", TokenType.BREAK)

case class DoubleToken() extends PrimitiveType("double", TokenType.PRIMITIVE)

case class ProtectedToken() extends Modifier("protected", TokenType.PROTECTED)

case class ByteToken() extends PrimitiveType("byte", TokenType.PRIMITIVE)

case class IfToken() extends Token("if", TokenType.IF)

case class ElseToken() extends Token("else", TokenType.ELSE)

case class CaseToken() extends Token("case", TokenType.CASE)

case class EnumToken() extends Token("enum", TokenType.ENUM)

case class DefaultToken() extends Token("default", TokenType.DEFAULT)

case class ReturnToken() extends Token("return", TokenType.RETURN)

case class SuperToken() extends Token("super", TokenType.SUPER)

case class PublicToken() extends Modifier("public", TokenType.PUBLIC)

case class CharToken() extends PrimitiveType("char", TokenType.PRIMITIVE)

case class IntToken() extends PrimitiveType("int", TokenType.PRIMITIVE)

case class ShortToken() extends PrimitiveType("short", TokenType.PRIMITIVE)


case class VoidToken() extends PrimitiveType("void", TokenType.PRIMITIVE)

case class FinalToken() extends Modifier("final", TokenType.FINAL) with VariableModifier

case class StaticToken() extends Modifier("static", TokenType.STATIC)

case class WhileToken() extends Token("while", TokenType.WHILE)

case class AndToken() extends BinOpToken("and", TokenType.AND)

case class OrToken() extends BinOpToken("or", TokenType.OR)


case class PlusToken() extends BinOpToken("+", TokenType.PLUS)

case class MinusToken() extends BinOpToken("-", TokenType.MINUS)

case class DivToken() extends BinOpToken("/", TokenType.DIV)

case class MultiplyToken() extends BinOpToken("*", TokenType.MULTIPLY)

case class PowToken() extends BinOpToken("^", TokenType.POWER)


case class AssignToken() extends Token("=", TokenType.ASSIGN)

case class LtToken() extends BinOpToken("<", TokenType.LT)

case class LteToken() extends BinOpToken("<=", TokenType.LTE)

case class GtToken() extends BinOpToken(">", TokenType.GT)

case class GteToken() extends BinOpToken(">=", TokenType.GTE)

case class EqToken() extends BinOpToken("==", TokenType.EQ)

case class NeqToken() extends BinOpToken("!=", TokenType.NEQ)

case class NotToken() extends Token("!", TokenType.NOT)

case class CommaToken() extends Token(",", TokenType.COMMA)

case class SemicolonToken() extends Token(";", TokenType.SEMICOLON) with Stmt

case class ColonToken() extends Token(":", TokenType.COLON)

case class DotToken() extends BinOpToken(".", TokenType.DOT)


case class QuotationToken() extends Token("\"", TokenType.QUOTATION)

case class LParenToken() extends Token("(", TokenType.LPAREN)

case class RParenToken() extends Token(")", TokenType.RPAREN)

case class LBraceToken() extends Token("[", TokenType.LBRACE)

case class RBraceToken() extends Token("]", TokenType.RBRACET)

case class LBracketToken() extends Token("{", TokenType.LBRACKET)

case class RBracketToken() extends Token("}", TokenType.RBRACKET)

case class NumberToken(override val value: String) extends LiteralToken(value)

case class FloatNumberToken(override val value: String) extends LiteralToken(value)

case class IdToken(override val value: String) extends Token(value, TokenType.ID) with Exp

case class NoToken() extends Token("", TokenType.NOTOKEN)

sealed abstract class LiteralToken(override val value: String) extends Token(value, TokenType.LITERAL)

case class StringToken(override val value: String) extends LiteralToken(value)

case class NullToken() extends LiteralToken("null")

case class FalseToken() extends LiteralToken("false")

case class TrueToken() extends LiteralToken("true")