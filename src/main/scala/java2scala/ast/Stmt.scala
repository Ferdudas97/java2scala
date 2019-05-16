package java2scala.ast

sealed trait Stmt extends Node

case class IfStatement(condition: Exp, stmt: Stmt, elseStmt: Option[Stmt]) extends Stmt

case class WhileStatement(condition: Exp, stmt: Stmt) extends Stmt

case class For(condition: Exp, stmt: Stmt) extends Stmt

case class BreakStatement()  extends Stmt

case class ReturnStatement(exp: Exp) extends Stmt