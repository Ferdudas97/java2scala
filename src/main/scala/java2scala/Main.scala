package java2scala

import java2scala.generator.RavaCompiler
import java2scala.keywords.{NoToken, PrivateToken, Token}
import java2scala.lexer.TokenRegex

object Main {

  def main(args: Array[String]): Unit = {
    RavaCompiler.files(args(0))
  }

}
