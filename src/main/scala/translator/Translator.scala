package cavaj
package translator

import scala.collection.Seq

import ir.*
import ast.*

private val INDENT = "  "

private def mkIndent(depth: Int): String = INDENT * depth

def translate(c: AstClass): String = {
  val quals = c.qualifiers.mkString("", " ", " ")
  val extend = c.extendsClass filterNot
    { _ == "java.lang.Object" } map
    { x => s" extends $x" } getOrElse ""
  val implements =
    if c.implements.nonEmpty
    then c.implements.mkString(" implements ", ", ", "")
    else ""
  val fields = c.fields.values.map { INDENT + _ + ";" }.mkString("\n")
  val fieldMethSep =
    if c.fields.nonEmpty && c.methods.values.exists { _.nonEmpty }
    then "\n\n"
    else ""
  val methods = c.methods.values.flatten
    .map { m =>
      val actualMethod = if m.name == "<init>" then m.copy(name = c.name) else m
      translate(actualMethod).map { INDENT + _ }.mkString("\n")
    }
    .mkString("\n\n")
  s"${quals}class ${c.name}$extend$implements {\n$fields$fieldMethSep$methods\n}"
}

def translate(m: AstMethod): Iterable[String] = {
  val quals = m.qualifiers.mkString("", " ", " ")
  val params = m.parameters.iterator
    .map { (name, ty) => s"$ty $name" }
    .mkString(", ")
  val signature =
    s"$quals${m.rettype} ${m.name}($params)"
  m.body match
    case None       => (signature + ";") :: Nil
    case Some(body) => (signature + " {") +: body.flatMap { translate(_, 1) } :+ "}"
}

def translate(s: Stmt, depth: Int = 0): Seq[String] = {
  s match
    case BlockStmt(stmts) =>
      (mkIndent(depth) + "{") +:
        stmts.flatMap { translate(_, depth + 1) } :+
        (mkIndent(depth) + "}")
    case VarDeclStmt(ty, name, value) => (mkIndent(depth) + s"$ty $name = $value;") :: Nil
    case ExprStmt(e)                  => (mkIndent(depth) + e + ";") :: Nil
    case VoidReturnStmt               => (mkIndent(depth) + "return;") :: Nil
    case ReturnStmt(v)                => (mkIndent(depth) + s"return $v;") :: Nil
    case IfStmt(cond, onTrue, onFalse) =>
      (mkIndent(depth) + s"if ($cond)") +: translate(onTrue, depth + 1) :++
        (onFalse.map { (mkIndent(depth) + "else") +: translate(_, depth + 1) }.getOrElse(Nil))
    case WhileStmt(cond, body) => (mkIndent(depth) + s"while ($cond)") +: translate(body, depth + 1)
}
