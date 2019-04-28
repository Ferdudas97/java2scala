package java2scala.keywords


sealed abstract class Token(val value: String, val line: Int = 0)

case class ClassToken(override val line: Int) extends Token("class", line = line)

case class AbstractToken(override val line: Int) extends Token("abstract", line = line)

case class ContinueToken(override val line: Int) extends Token("continue", line = line)

case class ForToken(override val line: Int) extends Token("for", line)

case class DoToken(override val line: Int) extends Token("do", line)

case class NewToken(override val line: Int) extends Token("new", line)

case class SwitchToken(override val line: Int) extends Token("switch", line)

case class PackageToken(override val line: Int) extends Token("package", line)

case class BooleanToken(override val line: Int) extends Token("boolean", line)

case class PrivateToken(override val line: Int) extends Token("private", line)

case class ThisToken(override val line: Int) extends Token("this", line)

case class BreakToken(override val line: Int) extends Token("break", line)

case class DoubleToken(override val line: Int) extends Token("double", line)

case class ProtectedToken(override val line: Int) extends Token("protected", line)

case class ByteToken(override val line: Int) extends Token("byte", line)

case class IfToken(override val line: Int) extends Token("if", line)

case class ElseToken(override val line: Int) extends Token("else", line)

case class CaseToken(override val line: Int) extends Token("case", line)

case class EnumToken(override val line: Int) extends Token("enum", line)

case class ReturnToken(override val line: Int) extends Token("return", line)

case class SuperToken(override val line: Int) extends Token("super", line)

case class PublicToken(override val line: Int) extends Token("public", line)

case class CharToken(override val line: Int) extends Token("char", line)

case class IntToken(override val line: Int) extends Token("int", line = line)

case class ShortToken(override val line: Int) extends Token("short", line)

case class VoidToken(override val line: Int) extends Token("void", line)

case class FinalToken(override val line: Int) extends Token("final", line)

case class WhileToken(override val line: Int) extends Token("while", line)

case class AndToken(override val line: Int) extends Token("and", line)

case class OrToken(override val line: Int) extends Token("or", line)


case class PlusToken(override val line: Int) extends Token("+", line)

case class MinusToken(override val line: Int) extends Token("-", line)

case class DivToken(override val line: Int) extends Token("/", line)

case class MultiplyToken(override val line: Int) extends Token("*", line)

case class PowToken(override val line: Int) extends Token("^", line)


case class AssignToken(override val line: Int) extends Token("=", line)

case class LtToken(override val line: Int) extends Token("<", line)

case class LteToken(override val line: Int) extends Token("<=", line)

case class GtToken(override val line: Int) extends Token(">", line)

case class GteToken(override val line: Int) extends Token(">=", line)

case class EqToken(override val line: Int) extends Token("==", line)

case class NeqToken(override val line: Int) extends Token("!=", line)

case class NotToken(override val line: Int) extends Token("!", line)


case class CommaToken(override val line: Int) extends Token(",", line)

case class SemicolonToken(override val line: Int) extends Token(";", line)

case class ColonToken(override val line: Int) extends Token(".", line)

case class QuotationToken(override val line: Int) extends Token("\"", line)

case class LParenToken(override val line: Int) extends Token("(", line)

case class RParenToken(override val line: Int) extends Token(")", line)

case class LBraceToken(override val line: Int) extends Token("[", line)

case class RBraceToken(override val line: Int) extends Token("]", line)

case class LBracketToken(override val line: Int) extends Token("{", line)

case class RBracketToken(override val line: Int) extends Token("}", line)

case class NumberToken(override val value: String, override val line: Int) extends Token(value, line)

case class FloatNumberToken(override val value: String, override val line: Int) extends Token(value, line)

case class IdToken(override val value: String, override val line: Int) extends Token(value, line)

case class NoToken(override val line: Int) extends Token(" ", line)

case class StringToken(override val value: String, override val line: Int) extends Token(value, line)
