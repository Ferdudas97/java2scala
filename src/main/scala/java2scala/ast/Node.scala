package java2scala.ast

import java2scala.keywords._

sealed trait Node


case class CompilationUnit(packageDeclaration: PackageDeclaration, importList: List[ImportDeclaration], typeDeclaration: TypeDeclaration)


case class PackageDeclaration(qualifiedName: QualifiedName) extends Node()

case class ImportDeclaration(qualifiedName: QualifiedName) extends Node()


sealed abstract class BinOp(val left: Node, val token: Token, val right: Node) extends Node()


case class PlusOp(override val left: Node, override val token: PlusToken, override val right: Node) extends BinOp(left, token, right)

case class MinusOp(override val left: Node, override val token: MinusToken, override val right: Node) extends BinOp(left, token, right)

case class DivOp(override val left: Node, override val token: DivToken, override val right: Node) extends BinOp(left, token, right)

case class MulOp(override val left: Node, override val token: MultiplyToken, override val right: Node) extends BinOp(left, token, right)

case class EqOp(override val left: Node, override val token: EqToken, override val right: Node) extends BinOp(left, token, right)

case class NeqOp(override val left: Node, override val token: NeqToken, override val right: Node) extends BinOp(left, token, right)

case class AndOp(override val left: Node, override val token: AndToken, override val right: Node) extends BinOp(left, token, right)

case class OrOp(override val left: Node, override val token: OrToken, override val right: Node) extends BinOp(left, token, right)

case class QualifiedName(name: List[IdToken]) extends Node

case class TypeDeclaration();
case class ClassOrInterfaceModifier(modifier : Modifier) extends Node
case class ClassOrInterfaceType(name: List[IdToken]) extends Node