package java2scala.ast

import java2scala.keywords._

sealed trait Exp extends Node


sealed trait Literal extends Exp

//
case class BinOp(left: Exp, token: BinOpToken, right: Exp) extends Exp


//case class PlusOp(override val left: Node, override val token: PlusToken, override val right: Node) extends BinOp(left, token, right)
//
//case class MinusOp(override val left: Node, override val token: MinusToken, override val right: Node) extends BinOp(left, token, right)
//
//case class DivOp(override val left: Node, override val token: DivToken, override val right: Node) extends BinOp(left, token, right)
//
//case class MulOp(override val left: Node, override val token: MultiplyToken, override val right: Node) extends BinOp(left, token, right)
//
//case class EqOp(override val left: Node, override val token: EqToken, override val right: Node) extends BinOp(left, token, right)
//
//case class NeqOp(override val left: Node, override val token: NeqToken, override val right: Node) extends BinOp(left, token, right)
//
//case class AndOp(override val left: Node, override val token: AndToken, override val right: Node) extends BinOp(left, token, right)
//
//case class OrOp(override val left: Node, override val token: OrToken, override val right: Node) extends BinOp(left, token, right)


case class IntegerLiteral(value: Int) extends Literal

case class FloatLiteral(value: Float) extends Literal

case class StringLiteral(value: String) extends Literal

case class NullLiteral() extends Literal

case class BooleanLiteral(boolean: Boolean) extends Literal

case class Assingment(exp1: Exp, exp2: Exp) extends Exp