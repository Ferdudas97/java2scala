package java2scala.generator

import java2scala.ast._
import java2scala.keywords._

object Visitator {


  def visit(compilationUnit: CompilationUnit): String = {

    val package_ = visit(compilationUnit.packageDeclaration)
    val imports = visit(compilationUnit.importDeclarations)
    val type_ = visit(compilationUnit.typeDeclaration)
    s"$package_ \n$imports \n$type_"

  }
  def visit(declarations: ImportDeclarations): String = {
    declarations.importList.map(i => i.qualifiedName.name)
      .map(ids => ids.map(p => p.value).mkString("import ", ".", ""))
      .mkString("\n")
  }

  def visit(declaration: PackageDeclaration): String = {
    declaration.qualifiedName.name.map(p => p.value).mkString("package ", ".", "\n")
  }

  def visit(declaration: TypeDeclaration): String = {
    declaration match {
      case clazz: ClassDeclaration => visitClass(clazz)
      case i: InterfaceDeclaration => visitInterface(i)
    }
  }

  def visitInterface(declaration: InterfaceDeclaration): String = {

    val modifier = declaration.modifier.map(visit).mkString(" ")
    val name = visit(declaration.name)
    val parents = if (declaration.parents.nonEmpty) declaration.parents.map(visit).mkString("extends ", " with ", "") else ""
    val body = declaration.body.map(visit).mkString("{\n", "\n", "}")
    s"$modifier trait $name $parents $body"
  }

  def visit(declaration: InterfaceMemberDeclaration): String = {
    val typeType = visit(declaration.typeType)
    val name = declaration.name.value
    val parameters = visit(declaration.formalParameter)
    s"def $name $parameters $typeType"
  }

  def visitClass(declaration: ClassDeclaration): String = {
    val modifier = declaration.modifier.map(visit).mkString(" ")
    val name = visit(declaration.name)
    val parents = declaration.parents.map(visit).mkString("extends ", " with ", "")
    val nonstaticBody = declaration.body.filter(p => !p.modifier.modifier.contains(StaticToken())).map(visit).mkString("{\n", "\n", "}")
    val staticBody = declaration.body.filter(p => p.modifier.modifier.contains(StaticToken())).map(visit).mkString("{\n", "\n", "}")

    s"""$modifier class $name $parents $nonstaticBody
       |object $name $staticBody
     """.stripMargin
  }


  def visit(memberDeclaration: ClassMemberDeclaration): String = {
    memberDeclaration match {
      case f: FieldDeclaration => visitField(f)
      case m: MethodDeclaration => visitMethod(m)
      case c: ConstructorDeclaration => visitConstructor(c)
    }
  }

  def visitMethod(m: MethodDeclaration): String = {
    val modifier = visit(m.modifier)
    val typeType = visit(m.typeType)
    val name = visit(m.name)
    val parameters = visit(m.formalParameters)
    val body = visit(m.body)
    s"$modifier def $name$parameters $typeType = {\n $body \n}"
  }


  def visit(body: MethodBody): String = {
    visit(body.block)
  }


  def visit(block: Block): String = block.blockStatement.map(visit).mkString("\n")

  def visit(blockStatement: BlockStatement): String = {
    blockStatement.stmt match {
      case l: LocalVariableDeclaration => visit(l)
      case f: Stmt => visitStmt(f)
    }
  }

  def visitStmt(stmt: Stmt): String = {
    stmt match {
      case f: ForStatement => visit(f)
      case r: ReturnStatement => s"return ${visit(r.exp)}"
      case s: SwitchStatement => visit(s)
      case i: IfStatement => visit(i)
      case w: WhileStatement => visit(w)
      case exp: Exp => visit(exp)
      case b: Block => visit(b)
      case b: BlockStatement => visit(b)
      case s: SemicolonToken => ""
      case breakStatement: BreakStatement => throw new RuntimeException("Break is not supported")
    }
  }


  def visit(statement: WhileStatement): String = {
    val condition = visit(statement.condition)
    val firststmt = visitStmt(statement.stmt)
    s"while ($condition) {\n$firststmt\n}\n"
  }

  def visit(statement: IfStatement): String = {
    val condition = visit(statement.condition)
    val firststmt = visitStmt(statement.stmt)
    val elseStmt = statement.elseStmt.map(visitStmt)
    s"if ($condition) {\n$firststmt\n}\n" + elseStmt.map(s => s"else {\n$s\n}").getOrElse("")
  }

  def visit(statement: SwitchStatement): String = {
    s"${visit(statement.condition)} match {\n ${visit(statement.switchGroups)} }\n"
  }

  def visit(groups: List[SwitchGroup]): String = {
    groups.map(visit).mkString("\n")
  }

  def visit(switchGroup: SwitchGroup): String = {
    val label = visit(switchGroup.switchLabel)
    val statement = visit(switchGroup.blockStatement)
    s"case $label  => { \n $statement }"
  }

  def visit(label: SwitchLabel): String = {
    label.condition.map(visit).getOrElse("_")
  }


  def visit(forStatement: ForStatement): String = {
    val declaration = visit(forStatement.forControl.init.localVariableDeclaration)
    val condition = visit(forStatement.forControl.exp)
    val block = visit(forStatement.block)
    val inc = forStatement.forControl.update.map(visit).mkString("\n")
    s"""{
       |$declaration
       |while($condition) {
       |  $block
       |  $inc
       |  }
       |}
       |
 """.stripMargin
  }


  def visitConstructor(declaration: ConstructorDeclaration): String = {
    val modifier = visit(declaration.modifier)
    val parameters = visit(declaration.parameters)
    val body = visit(declaration.block)
    s"$modifier def this$parameters = {\n this()\n $body } "
  }

  def visit(declaration: LocalVariableDeclaration): String = {
    val modifier = if (declaration.isFinal) "val" else "var"
    val names = declaration.declaratorList
      .map(d => d.variableDeclaratorId.idToken)
      .map(visit)
      .mkString(",")
    val typeType = visit(declaration.typeType)
    val values = declaration.declaratorList
      .map(d => d.initializer)
      .map(visit)
    val common = s"$modifier $names $typeType "
    values.size match {
      case 1 => s"$common = " + values.mkString("")
      case _ => s"$common = " + values.mkString("(", ", ", ")")
    }
  }

  def visit(parameters: FormalParameters): String = {
    parameters.parameters.map(visit).mkString("(", ", ", ")")
  }

  def visit(parameter: FormalParameter): String = {
    parameter.name.value + visit(parameter.typ)
  }

  def visitField(fieldDeclaration: FieldDeclaration): String = {
    val common = visitField(fieldDeclaration.modifier) +
      fieldDeclaration.declarators
        .map(d => d.variableDeclaratorId.idToken)
        .map(visit)
        .mkString(",") + visit(fieldDeclaration.typeType)
    val declarator = fieldDeclaration.declarators
      .map(d => d.initializer)
      .map(visit)
    declarator.size match {
      case 1 => s"$common = " + declarator.mkString("")
      case _ => s"$common = " + declarator.mkString("(", ", ", ")")
    }
  }


  def visit(initializer: Option[VariableInitializer]): String = {
    val matcher: VariableInitializer => String = {
      case a: ArrayInitializer => visit(a)
      case e: VariableInitializerByExpression => visit(e)
    }
    initializer.map(matcher).getOrElse("_")
  }

  def visit(variableInitializerByExpression: VariableInitializerByExpression): String = visit(variableInitializerByExpression.exp)

  def visit(exp: Exp): String = {
    exp match {
      case a: ArrayGet => s"${visit(a.exp1)}({visit(a.exp1)})"
      case p: ParExp => s"(${visit(p.exp)})"
      case p: BinOp => visit(p)
      case l: Literal => visit(l)
      case a: Assingment => s"(${visit(a.exp1)} = ${visit(a.exp2)})"
      case x: IdToken => s"${x.value} "
      case m: MethodCall => visit(m)
      case n: NotExp => s"!${visit(n.exp)}"
      case c: Creator => visit(c)
    }
  }

  def visit(creator: Creator): String = {
    s"new ${creator.idToken.value} ${visit(creator.expressionList)}"
  }

  def visit(op: BinOp): String = {
    op.token match {
      case _: AndToken => s"${visit(op.left)} || ${visit(op.right)}"
      case _: AndToken => s"${visit(op.left)} && ${visit(op.right)}"
      case _: DotToken => s"${visit(op.left)}.${visit(op.right)}"
      case _ => s"${visit(op.left)} ${op.token.value} ${visit(op.right)}"
    }

  }

  def visit(methodCall: MethodCall): String = {
    methodCall.name.value + visit(methodCall.expressionList)
  }

  def visit(expressionList: ExpressionList): String = {
    expressionList.exps.map(visit).mkString("(", ", ", ")")
  }

  def visit(l: Literal): String = {
    l match {
      case s: StringLiteral => s.value
      case b: BooleanLiteral => b.boolean.toString
      case i: IntegerLiteral => i.value.toString
      case f: FloatLiteral => f.value.toString
      case n: NullLiteral => "null"
    }
  }

  def visit(arrayInitializer: ArrayInitializer): String = {
    val values = arrayInitializer.values.map(Option(_)).map(visit).mkString(", ")
    s"Array($values)"
  }

  def visit(variableDeclarator: VariableDeclarator): String = {
    variableDeclarator.variableDeclaratorId.idToken.value
  }

  def visit(typeType: TypeType): String = {

    val typ = visit(typeType.typ)

    def toArray(i: Int, t: String): String = if (i > 0) s"Array[${toArray(i - 1, t)}]" else t

    s": ${toArray(typeType.braceNumber, typ)} "
  }

  def visit(typ: Type): String = {
    typ match {
      case t: IntToken => "Int "
      case _: BooleanToken => "Boolean "
      case _: DoubleToken => "Double "
      case _: ShortToken => "Short "
      case _: CharToken => "Char "
      case _: ByteToken => "Byte "
      case _: VoidToken => "Unit"
      case c: ClassOrInterfaceType => c.name.map(n => n.value).mkString(".")


    }
  }

  def visit(idToken: IdToken) = idToken.value + " "

  def visit(declaration: ClassOrInterfaceModifier): String = {
    declaration.modifier.map(visitMethodOrClassModifier).mkString(" ")
  }

  def visitMethodOrClassModifier(modifier: Modifier) = modifier match {
    case mod: PrivateToken => "private"
    case _: PublicToken => ""
    case _: AbstractToken => "abstract"
    case _: FinalToken => "final"
    case _: ProtectedToken => "protected"
    case _: StaticToken => ""
    case _: OverrideToken => "override"

  }

  def visitField(declaration: ClassOrInterfaceModifier): String = {
    val modifiers = declaration.modifier.map(visitFieldModifier).mkString(" ")
    if (modifiers.contains("val")) {
      val modified = modifiers.replace("val", "")
      modified + "val "
    }
    else {
      modifiers + " var "
    }
  }

  def visitFieldModifier(modifier: Modifier) = modifier match {
    case mod: PrivateToken => "private"
    case _: PublicToken => ""
    case _: AbstractToken => "abstract"
    case _: FinalToken => "val"
    case _: ProtectedToken => "protected"
    case _: StaticToken => ""
  }

}
