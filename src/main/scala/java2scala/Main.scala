package java2scala

import java2scala.lexer.TokenRegex

object Main {

  def main(args: Array[String]): Unit = {
    val text = "12314adsa"
    val t = TokenRegex.isFloatNumber.findPrefixMatchOf(text)
    t.foreach(println(_))
  }
}
