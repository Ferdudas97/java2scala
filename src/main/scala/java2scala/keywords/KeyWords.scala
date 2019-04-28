package java2scala.keywords

object KeyWords extends Enumeration {
  type KeyWords = Value
  val ABSTRACT, CONTINUE, FOR,
  NEW,
  SWITCH,
  IF,
  PACKAGE,
  BOOLEAN,
  DO,
  PRIVATE,
  THIS,
  BREAK,
  DOUBLE,
  IMPLEMENTS,
  PROTECTED,
  THROW,
  BYTE,
  ELSE,
  CASE,
  ENUM,
  RETURN,
  SUPER,
  PUBLIC,
  CHAR,
  SHORT,
  VOID,
  FINAL,
  WHILE,

  NUMBER,
  FLOAT_NUMBER,

  ID,
  LT,
  GT,
  LTE,
  GTE,
  EQ,
  NEQ,
  NOT,

  COMMA,
  SEMICOLON,
  COLON,

  LPAREN,
  RPAREN,
  LBRACE,
  RBRACET,
  LBRACKET,
  RBRACKET = Value
  val keywords = Map("abstract" -> ABSTRACT,
    "new" -> NEW,
  "switch" -> SWITCH,
    "if"-> IF,
  )
}

