package cavaj
package translator

import scala.collection.Seq
import ir.*

import ast.*


def translateStmt(stmt: Stmt): String =
  stmt match
    case VarDeclStmt(ty, name, value) => s"$ty $name = $value"
    case ExprStmt(value) => ""
    case BlockStmt(body) => "{\n" + body.map(translateStmt).mkString("", ";\n", ";\n") + "}"
    case IfStmt(cond, onTrue, onFalse) => s"if ($cond) \n\t$onTrue ${onFalse.map("else " + _).getOrElse("")}"
    case WhileStmt(l, cond, body) => s"l$l: while ($cond) $body"
    case DoWhileStmt(l, cond, body) => s"l$l: do $body while ($cond)"
    case ForStmt(l, init, cond, step, body) => {
      val initStr = init match
        case v: VarDeclStmt => translateStmt(v)
        case s: Seq[?] => s.mkString(", ")

      ???
    }
    case ForEachStmt(l, iterator, iterable, body) => s"l$l: for ($iterator : $iterable) $body"
    case BreakStmt(l) => s"break l$l"
    case ContinueStmt(l) => s"continue l$l"
    case VoidReturnStmt => "return"
    case ReturnStmt(v) => s"return $v"



def translateMethod(method: AstMethod): String = {
  val quals = method.qualifiers.filter(_ != Qualifier.Default).mkString("  ", " ", " ")
  val params = method.parameters.map { (name, ty) => s"$ty $name" }.mkString(", ")
  val bodys = method.body.map(s => s"{\n${s.map(translateStmt).mkString("", ";", ";")}\n}").getOrElse(";")
  s"$quals${method.rettype} ${method.name}($params)$bodys"
}



def translateClass(Class: AstClass): String = {
  val quals = Class.qualifiers.filter(_ != Qualifier.Default).mkString("", " ", " ")
  val extendss = if Class.extendsClass.isDefined then s" extends ${Class.extendsClass}" else ""
  val implementss = if Class.implements.nonEmpty then Class.implements.mkString(", ") else ""
  val fieldss = Class.fields.values.map(" " + _.toString + ";").mkString("\n")
  val methodss = Class.methods.values.flatten.map(translateMethod).mkString("\n")
  s"${quals}class ${Class.name}$extendss$implementss {\n$fieldss\n$methodss\n}"
}


def translateInterface(interface: AstInterface): String = {
  val quals = interface.qualifiers.filter(_ != Qualifier.Default).mkString("", " ", " ")
  val fieldss = interface.fields.values.map(" " + _.toString + ";").mkString("\n")
  val methodss = interface.methods.values.flatten.map(translateMethod).mkString("\n\n")
  val extendss = interface.implements.map(e => s" extends $e").mkString(" ")
  s"${quals}interface ${interface.name}$extendss {\n$fieldss\n\n$methodss\n}"
}

def translatePackage(packagee: AstPackage): String = {
  val interfaces = packagee.interfaces.values.map(i => translateInterface(i)).mkString("\n\n")
  val classes = packagee.classes.values.map(i => translateClass(i)).mkString("\n\n")
  s"$interfaces\n\n$classes"
}