package java2scala.ast

import java2scala.keywords._

trait Node

trait Type extends Node

case class ImportDeclarations(importList: List[ImportDeclaration]) extends Node
case class CompilationUnit(packageDeclaration: PackageDeclaration, importDeclarations: ImportDeclarations, typeDeclaration: TypeDeclaration) extends Node


case class PackageDeclaration(qualifiedName: QualifiedName) extends Node()

case class ImportDeclaration(qualifiedName: QualifiedName) extends Node()


case class QualifiedName(name: List[IdToken]) extends Node

sealed trait TypeDeclaration extends Node

case class ClassDeclaration(modifier: List[ClassOrInterfaceModifier], name: IdToken, body: List[ClassMemberDeclaration]) extends TypeDeclaration

sealed abstract class ClassMemberDeclaration(modifier: ClassOrInterfaceModifier) extends Node

case class FieldDeclaration(modifier: ClassOrInterfaceModifier, typeType: TypeType, declarators: List[VariableDeclarator]) extends ClassMemberDeclaration(modifier)

case class MethodDeclaration(modifier: ClassOrInterfaceModifier, typeType: TypeType, name: IdToken, formalParameters: FormalParameters, body: MethodBody) extends ClassMemberDeclaration(modifier)

case class ConstructorDeclaration(modifier: ClassOrInterfaceModifier, name: IdToken, parameters: FormalParameters, block: Block) extends ClassMemberDeclaration(modifier)

case class MethodBody(block: Block) extends Node

case class Creator(idToken: IdToken, expressionList: ExpressionList)

case class ClassBody(fieldsAndMethods: List[ClassMemberDeclaration])

case class ClassOrInterfaceModifier(modifier: List[Modifier]) extends Node

case class ClassOrInterfaceType(name: List[IdToken]) extends Type

case class TypeType(typ: Type, braceNumber: Int = 0) extends Type

case class VariableDeclaratorId(idToken: IdToken, bracesSize: Int = 0) extends Node

case class TypeOrVoid(node: Either[VoidToken, TypeType]) extends Node

case class FormalParameters(parameters: List[FormalParameter]) extends Node

case class FormalParameter(modifier: Option[VariableModifier], typ: TypeType, name: IdToken) extends Node


