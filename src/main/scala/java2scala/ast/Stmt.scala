package java2scala.ast

import java2scala.keywords.{FinalToken, IdToken}

sealed trait Stmt extends Node

case class IfStatement(condition: Exp, stmt: Stmt, elseStmt: Option[Stmt]) extends Stmt

case class WhileStatement(condition: Exp, stmt: Stmt) extends Stmt

case class For(condition: Exp, stmt: Stmt) extends Stmt

case class BreakStatement() extends Stmt

case class ReturnStatement(exp: Exp) extends Stmt

case class Block(blockStatement: List[BlockStatement]) extends Stmt

case class BlockStatement(stmt: Stmt)

case class LocalVariableDeclaration(isFinal: Boolean, typeType: TypeType, declaratorList: List[VariableDeclarator]) extends Stmt

sealed abstract class VariableInitializer() extends Stmt

case class ArrayInitializer(values: List[VariableInitializer]) extends VariableInitializer

case class VariableInitializerByExpression(exp: Exp) extends VariableInitializer

case class VariableDeclarator(variableDeclaratorId: VariableDeclaratorId, initializer: Option[VariableInitializer])