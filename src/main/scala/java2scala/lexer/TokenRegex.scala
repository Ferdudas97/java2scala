package java2scala.lexer

import scala.util.matching.Regex

object TokenRegex {
  val isNumber: Regex = "[0-9]+".r
  val isFloatNumber: Regex = """[0-9]+\.[0-9]+""".r
  val isIdentifier: Regex = "[a-zA-z]+[a-zA-z0-9]*".r
  val isLetter : Regex = "[a-zA-Z]".r
  val lineComment :Regex = "//.*\n".r

}
