package cavaj
package translator

import scala.collection.Seq
import ir.*

import ast.*

//def translate(stmt: IfStmt): String =
//  s"if (${stmt.cond}) {\n\t${stmt.onTrue}} ${if stmt.onFalse.isDefined then s"""else ${stmt.onFalse}\n}""" else """"""}"
//
//
//def translate(stmt: WhileStmt): String =
//  s"while (${stmt.cond}) {\n\t${stmt.body}}"

def translate(stmt: Stmt): String =
  stmt match
    case VarDeclStmt(ty, name, value) => s"$ty $name = $value"
    case ExprStmt(value) => ""
    case BlockStmt(body) => s"{${body.map(translate).mkString(";\n")}}"
    case IfStmt(cond, onTrue, onFalse) => s"if ($cond) \n\t$onTrue ${onFalse.map("else " + _).getOrElse("")}"
    case WhileStmt(cond, body) => s"while ($cond) $body"
    case DoWhileStmt(cond, body) => s"do $body while ($cond)"
    case ForStmt(init, cond, step, body) => {
      val initStr = init match
        case v: VarDeclStmt => translate(v)
        case s: Seq[?] => s.mkString(", ")

      ???
    }
    case ForEachStmt(iterator, iterable, body) => s"for ($iterator : $iterable) $body"
    case BreakStmt => "break"
    case ContinueStmt => "continue"
    case _ => ???



def translate(method: AstMethod): String = {
  val quals = method.qualifiers.filter(_ != Qualifier.Default).mkString("  ", " ", " ")
  val params = method.parameters.map { case (name, ty) => s"$ty $name" }.mkString(", ")
  val bodys = method.body.map(s => s"{\n${translate(s)}\n}").getOrElse("")
  s"$quals${method.rettype}${method.name}($params)$bodys"
}



def translate(Class: AstClass): String = {
  val quals = Class.qualifiers.filter(_ != Qualifier.Default).mkString("", " ", " ")
  val extendss = if Class.extendsClass.isDefined then s" extends ${Class.extendsClass}" else ""
  val implementss = if Class.implements.nonEmpty then Class.implements.mkString(", ") else ""
  val fieldss = Class.fields.values.map(" " + _.toString + ";").mkString("\n")
  val methodss = Class.methods.values.flatten.map(_.toString).mkString("\n\n")
  s"${quals}class ${Class.name}$extendss$implementss {\n$fieldss\n\n$methodss\n}"
}


def translate(interface: AstInterface): String = {
  val quals = interface.qualifiers.filter(_ != Qualifier.Default).mkString("", " ", " ")
  val fieldss = interface.fields.values.map(" " + _.toString + ";").mkString("\n")
  val methodss = interface.methods.values.flatten.map(_.toString).mkString("\n\n")
  val extendss = interface.extendsInterface.map(e => s" extends $e").mkString(" ")
  s"${quals}interface ${interface.name}$extendss {\n$fieldss\n\n$methodss\n}"
}

def translate(packagee: AstPackage): String = {
  val interfaces = packagee.interfaces.values.map(i => translate(i: AstInterface)).mkString("\n\n")
  val classes = packagee.classes.values.map(i => translate(i: AstClass)).mkString("\n\n")
  s"$interfaces\n\n$classes"
}