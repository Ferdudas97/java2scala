package java2scala.generator

import java.io.{File, PrintWriter}
import java.util.{Observable, Observer}

import java2scala.ast.{CompilationUnit, Node}
import java2scala.lexer.Lexer
import java2scala.parser.Parser

import scala.io.Source

object RavaCompiler{

  val fileExtension = ".rava"

  def files(string: String): Unit = {
    val d = new File(string)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).filter(_.getName.contains(fileExtension)).foreach(generate)
      d.listFiles.filter(_.isDirectory).foreach(f => files(f.getPath))
    } else {
      List[File]()
    }
  }

  def generate(scalaContent: String, newPath: String): Unit = {
    val printer = new PrintWriter(new File(newPath))
    printer.println(scalaContent)
    printer.close()
  }

  def generate(file: File): Unit = {
    val name = file.getPath.replace(fileExtension, ".scala")
    val contet = Source.fromFile(file).mkString
    val scalaContent = compile(contet)
    val printer = new PrintWriter(new File(name))
    printer.println(scalaContent)
    printer.close()


  }


  def compile(string: String): String = Visitator.visit(Parser(Lexer.parse(string)).parse())

}
