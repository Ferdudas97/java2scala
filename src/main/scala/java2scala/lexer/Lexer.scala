package java2scala.lexer

import java2scala.keywords._

object Lexer{


  def parse(str: String): List[Token] = {
    parseText(prepareText(str))
  }

  private def parseText(text: String): List[Token] = {
    val token = readToken(text)
    token match {
      case Some(value) => value :: parseText(prepareText(text, value))
      case None => Nil
    }
  }


  private def prepareText(text: String, token: Token = NoToken()): String = {
    text.drop(token.value.length)
      .dropWhile(c => c.isWhitespace)
  }

  private def readToken(text: String) = {

    val charToken = parseChar(text)

    charToken
  }


  private def parseChar(text: String): Option[Token] = {
    if (text.length == 0) None else {
      val char = text.charAt(0)
      val token = text.charAt(0) match {
        case ';' => SemicolonToken()
        case ':' => ColonToken()
        case ',' => CommaToken()
        case '.' => DotToken()
        case '(' => LParenToken()
        case ')' => RParenToken()
        case '{' => LBracketToken()
        case '}' => RBracketToken()
        case '[' => LBraceToken()
        case ']' => RBraceToken()
        case '>' => if (text.charAt(1) == '=') GteToken() else GtToken()
        case '<' => if (text.charAt(1) == '=') LteToken() else LtToken()
        case '=' => if (text.charAt(1) == '=') EqToken() else AssignToken()
        case '!' => if (text.charAt(1) == '=') NeqToken() else NotToken()
        case '+' => PlusToken()
        case '-' => MinusToken()
        case '/' => DivToken()
        case '*' => MultiplyToken()
        case '^' => PowToken()
        case '"' => StringToken('"' + readString(text.substring(1)) + '"')
        case _ => parseString(text).getOrElse(NoToken())
      }
      Option(token)
    }
  }

  private def readString(text: String) = text.takeWhile(c => c != '"')

  private def parseString(text: String) = TokenRegex.isNumber.findPrefixMatchOf(text)
    .orElse(TokenRegex.isFloatNumber.findPrefixMatchOf(text))
    .orElse(TokenRegex.isIdentifier.findPrefixMatchOf(text))
    .orElse(throw new RuntimeException(text))
    .map(r => r toString)
    .map(s => tokenFromString(s))


  private def tokenFromString(string: String) = string match {
    case "and" => AndToken()
    case "or" => OrToken()
    case "while" => WhileToken()
    case "for" => ForToken()
    case "do" => DoToken()
    case "abstract" => AbstractToken()
    case "continue" => ContinueToken()
    case "new" => NewToken()
    case "switch" => SwitchToken()
    case "package" => PackageToken()
    case "boolean" => BooleanToken()
    case "private" => PrivateToken()
    case "this" => ThisToken()
    case "break" => BreakToken()
    case "double" => DoubleToken()
    case "protected" => ProtectedToken()
    case "interface" => InterfaceToken()
    case "override" => OverrideToken()
    case "byte" => ByteToken()
    case "else" => ElseToken()
    case "case" => CaseToken()
    case "if" => IfToken()
    case "enum" => EnumToken()
    case "return" => ReturnToken()
    case "super" => SuperToken()
    case "public" => PublicToken()
    case "char" => CharToken()
    case "short" => ShortToken()
    case "void" => VoidToken()
    case "final" => FinalToken()
    case "int" => IntToken()
    case "class" => ClassToken()
    case "import" => ImportToken()
    case "null" => NullToken()
    case "false" => FalseToken()
    case "true" => TrueToken()
    case "static"=> StaticToken()
    case TokenRegex.isNumber(_*) => NumberToken(string)
    case TokenRegex.isFloatNumber(_*) => FloatNumberToken(string)
    case TokenRegex.isIdentifier(_*) => IdToken(string)
  }

}
