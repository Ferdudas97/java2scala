package java2scala.keywords

import java2scala.keywords.TokenType.TokenType


sealed abstract class Token(val value: String,val typ :TokenType ){
  def is(tokenType: TokenType): Boolean = this.typ == tokenType;
}

sealed trait Modifier extends Token

sealed trait PrimitiveType extends Token

case class ClassToken() extends Token("class",TokenType.CLASS)

case class AbstractToken() extends Modifier("abstract",TokenType.ABSTRACT)

case class ContinueToken() extends Token("continue",TokenType.CONTINUE)

case class ForToken() extends Token("for", TokenType.FOR)

case class DoToken() extends Token("do",TokenType.DO)

case class NewToken() extends Token("new", TokenType.NEW)

case class SwitchToken() extends Token("switch", TokenType.SWITCH)

case class PackageToken() extends Token("package", TokenType.PACKAGE)

case class ImportToken() extends Token("import", TokenType.IMPORT)

case class BooleanToken() extends PrimitiveType("boolean", TokenType.BOOLEAN)

case class PrivateToken() extends Modifier("private", TokenType.PRIVATE)

case class ThisToken() extends Token("this", TokenType.THIS)

case class BreakToken() extends Token("break", TokenType.BREAK)

case class DoubleToken() extends PrimitiveType("double", TokenType.DOUBLE)

case class ProtectedToken() extends Modifier("protected", TokenType.PROTECTED)

case class ByteToken() extends PrimitiveType("byte", TokenType.BYTE)

case class IfToken() extends Token("if", TokenType.IF)

case class ElseToken() extends Token("else", TokenType.ELSE)

case class CaseToken() extends Token("case",TokenType.CASE)

case class EnumToken() extends Token("enum", TokenType.ENUM)

case class ReturnToken() extends Token("return", TokenType.RETURN)

case class SuperToken() extends Token("super", TokenType.SUPER)

case class PublicToken() extends Token("public", TokenType.PUBLIC)

case class CharToken() extends PrimitiveType("char", TokenType.CHAR)

case class IntToken() extends Token("int", TokenType.INT)

case class ShortToken() extends PrimitiveType("short", TokenType.SHORT)

case class VoidToken() extends Token("void",TokenType.VOID)

case class FinalToken() extends Modifier("final", TokenType.FINAL)

case class WhileToken() extends Token("while", TokenType.WHILE)

case class AndToken() extends Token("and", TokenType.AND)

case class OrToken() extends Token("or", TokenType.OR)


case class PlusToken() extends Token("+", TokenType.PLUS)

case class MinusToken() extends Token("-", TokenType.MINUS)

case class DivToken() extends Token("/", TokenType.DIV)

case class MultiplyToken() extends Token("*",TokenType.MULTIPLY)

case class PowToken() extends Token("^", TokenType.POWER)


case class AssignToken() extends Token("=", TokenType.ASSIGN)

case class LtToken() extends Token("<", TokenType.LT)

case class LteToken() extends Token("<=", TokenType.LTE)

case class GtToken() extends Token(">", TokenType.GT)

case class GteToken() extends Token(">=", TokenType.GTE)

case class EqToken() extends Token("==", TokenType.EQ)

case class NeqToken() extends Token("!=", TokenType.NEQ)

case class NotToken() extends Token("!", TokenType.NOT)

case class CommaToken() extends Token(",", TokenType.COMMA)

case class SemicolonToken() extends Token(";",TokenType.SEMICOLON)

case class ColonToken() extends Token(".",TokenType.COLON)

case class QuotationToken() extends Token("\"",TokenType.QUOTATION)

case class LParenToken() extends Token("(", TokenType.LPAREN)

case class RParenToken() extends Token(")",TokenType.RPAREN)

case class LBraceToken() extends Token("[",TokenType.LBRACE)

case class RBraceToken() extends Token("]",TokenType.RBRACET)

case class LBracketToken() extends Token("{",TokenType.LBRACKET)

case class RBracketToken() extends Token("}",TokenType.RBRACET)

case class NumberToken(override val value: String) extends Token(value,TokenType.NUMBER)

case class FloatNumberToken(override val value: String) extends Token(value,TokenType.FLOAT_NUMBER)

case class IdToken(override val value: String) extends Token(value, TokenType.ID)

case class NoToken() extends Token(" ", TokenType.NOTOKEN)

case class StringToken(override val value: String) extends Token(value,TokenType.STRING)
