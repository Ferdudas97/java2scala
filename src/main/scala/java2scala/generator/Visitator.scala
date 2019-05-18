package java2scala.generator

import java2scala.ast.{CompilationUnit, ImportDeclaration, PackageDeclaration, TypeDeclaration}

object Visitator {


  def visit(compilationUnit: CompilationUnit): String = {

    visit(compilationUnit.packageDeclaration) +
      visit(compilationUnit.importList) +
      visit(compilationUnit.typeDeclaration)

  }

  def visit(importList: List[ImportDeclaration]): String = {
    importList.map(i => i.qualifiedName.name)
      .map(ids => ids.mkString("import ", ".", ""))
      .mkString("\n")
  }

  def visit(declaration: PackageDeclaration): String = {
    declaration.qualifiedName.name.mkString("package ", ".", "")
  }

  def visit(declaration: TypeDeclaration): String = {
    ""
  }
}
