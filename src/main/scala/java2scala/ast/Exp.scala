package java2scala.ast

import java2scala.keywords._

trait Exp extends Node with Stmt


sealed trait Literal extends Exp

case class BinOp(left: Exp, token: BinOpToken, right: Exp) extends Exp

case class ArrayGet(exp1: Exp, exp2: Exp) extends Exp

case class ParExp(exp: Exp) extends Exp

case class ExpressionList(exps: List[Exp]) extends Exp

case class MethodCall(name: IdToken, expressionList: List[ExpressionList]) extends Exp

case class IntegerLiteral(value: Int) extends Literal

case class FloatLiteral(value: Float) extends Literal

case class StringLiteral(value: String) extends Literal

case class NullLiteral() extends Literal

case class BooleanLiteral(boolean: Boolean) extends Literal

case class Assingment(exp1: Exp, exp2: Exp) extends Exp

case class Creator(idToken: IdToken, expressionList: ExpressionList) extends Exp

case class NotExp(exp: Exp) extends Exp
