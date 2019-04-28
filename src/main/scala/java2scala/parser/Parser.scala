package java2scala.parser

import java2scala.keywords.{PackageToken, Token}

import scala.collection.mutable

class Parser {

  var index = 0
  var queue : mutable.Queue[Token]
  var token: Token = getToken()
  def parse(tokens: List[Token]) ={
    queue = mutable.Queue(tokens: _*)
  }

  def getToken() = queue.dequeue()

  def currentToken() : Token
  def advance() = token = getToken()
  def eat(tok: Token) = if (token == tok) advance()
  def packageDeclaration() = token match {
    case t : PackageToken => eat(PackageToken())
  }

  def compilationUnit() = packageDeclaration() importDeclaration() typeDeclaration()
}
