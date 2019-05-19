package java2scala.keywords

object TokenType extends Enumeration {

  type TokenType = Value
  val ABSTRACT,
  CONTINUE,
  FOR,
  CLASS,
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

  LITERAL,
  DEFAULT,
  NUMBER,
  FLOAT_NUMBER,
  INT,
  POWER,
  MINUS,
  PLUS,
  MULTIPLY,
  DIV,
  MOD,
  IMPORT,
  ID,
  LT,
  GT,
  LTE,
  GTE,
  EQ,
  NEQ,
  NOT,
  AND,
  OR,
  ASSIGN,
  NOTOKEN,
  STRING,
  QUOTATION,



  COMMA,
  SEMICOLON,
  COLON,
  DOT,



  LPAREN,
  RPAREN,
  LBRACE,
  RBRACET,
  LBRACKET,
  RBRACKET = Value;

}

