package java2scala

import java2scala.keywords.{NoToken, PrivateToken, Token}
import java2scala.lexer.TokenRegex

object Main {

  val token :Token = NoToken()
  def main(args: Array[String]): Unit = {
    val text = "12314adsa"
    val t = TokenRegex.isFloatNumber.findPrefixMatchOf(text)
    eat(NoToken.getClass)
  }
  def eat(obj : Class[_]) = if ( token.getClass == obj.getClass) println("true") else println("false")

}
