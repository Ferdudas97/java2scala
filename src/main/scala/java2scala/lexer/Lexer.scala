package java2scala.lexer

import java2scala.keywords._

class Lexer {


  var position = 0
  var line = 1

  def parse(text: String): List[Token] = {
    val token = readToken(text)
    token match {
      case Some(value) => value :: parse(prepareText(text, value))
      case None => Nil
    }
  }


  private def prepareText(text: String, token: Token): String = {
    val prepared = text.drop(token.value.length)
      .dropWhile(c => c == ' ')
    if (prepared.length > 0 && prepared.charAt(0) == '\n') {
      line = line + 1
      prepared.substring(1)

    } else prepared

  }

  private def readToken(text: String) = {

    val charToken = parseChar(text)

    charToken
  }


  private def parseChar(text: String): Option[Token] = {
    if (text.length == 0) None else {
      val char = text.charAt(0)
      val token = text.charAt(0) match {
        case ';' => SemicolonToken(line)
        case ',' => CommaToken(line)
        case '.' => ColonToken(line)
        case '(' => LParenToken(line)
        case ')' => RParenToken(line)
        case '{' => LBracketToken(line)
        case '}' => RBracketToken(line)
        case '(' => LParenToken(line)
        case ')' => RParenToken(line)
        case '>' => if (text.charAt(1) == '=') GteToken(line) else GtToken(line)
        case '<' => if (text.charAt(1) == '=') LteToken(line) else LtToken(line)
        case '=' => if (text.charAt(1) == '=') EqToken(line) else AssignToken(line)
        case '!' => if (text.charAt(1) == '=') NeqToken(line) else NotToken(line)
        case '+' => PlusToken(line)
        case '-' => MinusToken(line)
        case '/' => DivToken(line)
        case '*' => MultiplyToken(line)
        case '^' => PowToken(line)
        case '"' => StringToken('"' + readString(text.substring(1)) + '"', line)
        case _ => parseString(text).getOrElse(NoToken(line))
      }
      Option(token)
    }
  }

  private def readString(text: String) = text.takeWhile(c => c != '"')

  private def parseString(text: String) = TokenRegex.isNumber.findPrefixMatchOf(text)
    .orElse(TokenRegex.isFloatNumber.findPrefixMatchOf(text))
    .orElse(TokenRegex.isIdentifier.findPrefixMatchOf(text))
    .map(r => r toString)
    .map(s => tokenFromString(s))


  private def tokenFromString(string: String) = string match {
    case "and" => AndToken(line)
    case "or" => OrToken(line)
    case "while" => WhileToken(line)
    case "for" => ForToken(line)
    case "do" => DoToken(line)
    case "abstract" => AbstractToken(line)
    case "continue" => ContinueToken(line)
    case "new" => NewToken(line)
    case "switch" => SwitchToken(line)
    case "package" => PackageToken(line)
    case "boolean" => BooleanToken(line)
    case "private" => PrivateToken(line)
    case "this" => ThisToken(line)
    case "break" => BreakToken(line)
    case "double" => DoubleToken(line)
    case "protected" => ProtectedToken(line)
    case "byte" => ByteToken(line)
    case "else" => ElseToken(line)
    case "case" => ProtectedToken(line)
    case "if" => IfToken(line)
    case "enum" => EnumToken(line)
    case "return" => ReturnToken(line)
    case "super" => SuperToken(line)
    case "public" => PublicToken(line)
    case "char" => CharToken(line)
    case "short" => ShortToken(line)
    case "void" => VoidToken(line)
    case "final" => FinalToken(line)
    case "int" => IntToken(line)
    case "class" => ClassToken(line)
    case TokenRegex.isNumber(_*) => NumberToken(string, line)
    case TokenRegex.isFloatNumber(_*) => FloatNumberToken(string, line)
    case TokenRegex.isIdentifier(_*) => IdToken(string, line)
  }

}
