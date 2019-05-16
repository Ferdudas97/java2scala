package java2scala.ast

import java2scala.keywords._

trait Node

trait Type extends Node

case class CompilationUnit(packageDeclaration: PackageDeclaration, importList: List[ImportDeclaration], typeDeclaration: TypeDeclaration)


case class PackageDeclaration(qualifiedName: QualifiedName) extends Node()

case class ImportDeclaration(qualifiedName: QualifiedName) extends Node()




case class QualifiedName(name: List[IdToken]) extends Node

case class TypeDeclaration();

case class ClassDeclaration(modifier: ClassOrInterfaceModifier, name:IdToken)

case class ClassOrInterfaceModifier(modifier: Modifier) extends Node

case class ClassOrInterfaceType(name: List[IdToken]) extends Type

case class TypeType(typ: Type, braceNumber: Int = 0)


case class TypeOrVoid(node: Either[VoidToken, TypeType]) extends Node

case class FormalParameters(parameters : List[FormalParameter]) extends Node
case class FormalParameter(modifier: Option[VariableModifier], typ: TypeType, name : IdToken) extends Node


