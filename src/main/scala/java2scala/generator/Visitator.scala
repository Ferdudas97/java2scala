//package java2scala.generator
//
//import java2scala.ast._
//import java2scala.keywords._
//
//object Visitator {
//
//
//  def visit(compilationUnit: CompilationUnit): String = {
//
//    visit(compilationUnit.packageDeclaration) +
//      visit(compilationUnit.importList) +
//      visit(compilationUnit.typeDeclaration)
//
//  }
//
//  def visit(importList: List[ImportDeclaration]): String = {
//    importList.map(i => i.qualifiedName.name)
//      .map(ids => ids.mkString("import ", ".", ""))
//      .mkString("\n")
//  }
//
//  def visit(declaration: PackageDeclaration): String = {
//    declaration.qualifiedName.name.mkString("package ", ".", "")
//  }
//
//  def visit(declaration: TypeDeclaration): String = {
//    declaration match {
//      case clazz: ClassDeclaration => visitClass(clazz)
//      case _ => ""
//    }
//  }
//
//  def visitClass(declaration: ClassDeclaration): String = {
//    declaration.modifier.map(visit).mkString +
//      visit(declaration.name) + " { \n" +
//      declaration.body.map(visit).mkString
//  }
//
//
//  def visit(memberDeclaration: ClassMemberDeclaration): String = {
//    memberDeclaration match {
//      case f: FieldDeclaration => visitField(f)
//      case m: MethodDeclaration => visitMethod(m)
//      case c: ConstructorDeclaration => visitConstructor(c)
//    }
//  }
//
//  def visitMethod(m: MethodDeclaration): String = {
//    val modifier = visit(m.modifier)
//    val typeType = visit(m.typeType)
//    val name = visit(m.name)
//    val parameters = visit(m.formalParameters)
//    val body = visit(m.body)
//    s"$modifier def $name$parameters $typeType = {\n $body \n}"
//  }
//
//  def visit(body: MethodBody): String = {
//    visit(body.block)
//  }
//
//  val x, y: Int
//
//  def visit(block: Block): String = block.blockStatement.map(visit).mkString("\n")
//
//  def visit(blockStatement: BlockStatement): String = {
//    blockStatement.stmt match {
//      case l: LocalVariableDeclaration => visit(l)
//      case f: Stmt => visitStmt(f)
//    }
//  }
//
//  def visitStmt(stmt: Stmt): String = {
//    stmt match {
//      case f: ForStatement => visit(f)
//      case r: ReturnStatement => s"return ${visit(r.exp)}"
//      case s: SwitchStatement => visit(s)
//    }
//  }
//
//  def visit(statement: SwitchStatement): String = {
//    s"${visit(statement.condition)} match {\n ${visit(statement.switchGroups)} }\n"
//  }
//
//  def visit(groups: List[SwitchGroup]): String = {
//    groups.map(visit).mkString("\n")
//  }
//
//  def visit(switchGroup: SwitchGroup): String = {
//    val label = visit(switchGroup.switchLabel)
//    val statement = visit(switchGroup.blockStatement)
//    s"case $label  => { \n $statement }"
//  }
//
//  def visit(label: SwitchLabel): String = {
//    label.condition.map(visit).getOrElse("_")
//  }
//
//
//  def visit(forStatement: ForStatement): String = {
//    visit(forStatement.forControl)
//  }
//
//  def visit(control: ForControl): String = {
//    val declaration = control.init.localVariableDeclaration
//    val i
//
//  }
//
//  def visitConstructor(declaration: ConstructorDeclaration)
//
//  def visit(declaration: LocalVariableDeclaration): String = {
//    val modifier = if (declaration.isFinal) "val" else "var"
//    val names = declaration.declaratorList
//      .map(d => d.variableDeclaratorId.idToken)
//      .map(visit)
//      .mkString(",")
//    val typeType = visit(declaration.typeType)
//    val values = declaration.declaratorList
//      .map(d => d.initializer)
//      .map(visit)
//      .mkString("= (", ", ", ")")
//    s"$modifier $names $typeType $values \n"
//  }
//
//  def visit(parameters: FormalParameters): String = {
//    parameters.parameters.map(visit).mkString("(", ", ", ")")
//  }
//
//  def visit(parameter: FormalParameter): String = {
//    parameter.name.value + visit(parameter.typ)
//  }
//
//  def visitField(fieldDeclaration: FieldDeclaration): String = {
//    visit(fieldDeclaration.modifier) +
//      fieldDeclaration.declarators
//        .map(d => d.variableDeclaratorId.idToken)
//        .map(visit)
//        .mkString(",") +
//      visit(fieldDeclaration.typeType) +
//      fieldDeclaration.declarators
//        .map(d => d.initializer)
//        .map(visit)
//        .mkString("= (", ", ", ")")
//  }
//
//  def visit(declarators: List[VariableDeclarator]): String = {
//    declarators
//      .map(d => d.variableDeclaratorId.idToken)
//      .map(visit)
//      .mkString(",") +
//      declarators
//        .map(d => d.initializer)
//        .map(visit)
//        .mkString("= (", ", ", ")")
//  }
//
//  def visit(initializer: Option[VariableInitializer]): String = {
//    val matcher: VariableInitializer => String = {
//      case a: ArrayInitializer => visit(a)
//      case e: VariableInitializerByExpression => visit(e)
//    }
//    initializer.map(matcher).getOrElse("_")
//  }
//
//  def visit(variableInitializerByExpression: VariableInitializerByExpression): String = visit(variableInitializerByExpression.exp)
//
//  def visit(exp: Exp): String = {
//    exp match {
//      case p: ParExp => s"(${visit(p.exp)})"
//      case p: BinOp => s"(${visit(p.left)} ${p.token.value} ${visit(p.right)})"
//      case l: Literal => visit(l)
//      case a: Assingment => s"(${visit(a.exp1)} = ${visit(a.exp2)})"
//    }
//  }
//
//  def visit(l: Literal): String = {
//    l match {
//      case s: StringLiteral => s.value
//      case b: BooleanLiteral => b.boolean.toString
//      case i: IntegerLiteral => i.value.toString
//      case f: FloatLiteral => f.value.toString
//      case n: NullLiteral => "null"
//    }
//  }
//
//  def visit(arrayInitializer: ArrayInitializer): String = {
//    val values = arrayInitializer.values.map(Option(_)).map(visit).mkString(", ")
//    s"Array($values)"
//  }
//
//  def visit(variableDeclarator: VariableDeclarator): String = {
//    variableDeclarator.variableDeclaratorId.idToken.value
//  }
//
//  def visit(typeType: TypeType): String = {
//
//    val typ = visit(typeType.typ)
//
//    def toArray(i: Int, t: String): String = if (i > 0) s"Array[${toArray(i - 1, t)}]" else t
//
//    s": ${toArray(typeType.braceNumber, typ)} "
//  }
//
//  def visit(typ: Type): String = {
//    typ match {
//      case t: IntToken => "Int "
//      case _: BooleanToken => "Boolean "
//      case _: DoubleToken => "Double "
//      case _: ShortToken => "Short "
//      case _: CharToken => "Char "
//      case _: ByteToken => "Byte "
//      case c: ClassOrInterfaceType => c.name.map(n => n.value).mkString(".")
//
//
//    }
//  }
//
//  def visit(idToken: IdToken) = idToken.value + " "
//
//  def visit(declaration: ClassOrInterfaceModifier): String = {
//    declaration.modifier match {
//      case mod: PrivateToken => "private "
//      case _: PublicToken => "public "
//      case _: AbstractToken => "abstract "
//      case _: FinalToken => "final "
//      case _: ProtectedToken => "protected "
//    }
//  }
//}
