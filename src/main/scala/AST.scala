package cavaj
package ast

import ir.*

import scala.collection.Seq
import cavaj.Type

type AstClass  = Class[AstMethod]
type AstMethod = Method[Seq[Stmt]]
type AstInterface = Interface[AstMethod]
type AstPackage = Package[?] // TODO

type Expr = Value
sealed trait Stmt

case class VarDeclStmt(ty: Type, name: String, value: Expr) extends Stmt {
  override def toString: String = s"$ty $name = $value"
}

case class ExprStmt(value: Expr) extends Stmt {
  override def toString: String = value.toString
}

case class BlockStmt(stmts: Seq[Stmt]) extends Stmt {
  override def toString: String = stmts.mkString("{\n", ";\n", "}")
}

case object VoidReturnStmt extends Stmt {
  override def toString: String = "return"
}

case class ReturnStmt(value: Expr) extends Stmt {
  override def toString: String = s"return $value";
}

case class IfStmt(cond: Expr, onTrue: Stmt, onFalse: Option[Stmt]) extends Stmt {
  override def toString: String =
    s"if ($cond)\n$onTrue" + onFalse.map { "\nelse " + _.toString }.getOrElse("") + "\n"
}

case class WhileStmt(label: BbIndex, cond: Expr, body: Stmt) extends Stmt {
  override def toString: String = s"l$label: while ($cond) $body"
}

case class DoWhileStmt(label: BbIndex, cond: Expr, body: Stmt) extends Stmt

case class ForStmt(
    label: BbIndex,
    init: VarDeclStmt | Seq[Expr],
    cond: Option[Expr],
    step: Seq[Expr],
    body: Stmt,
) extends Stmt

case class ForEachStmt(label: BbIndex, iterator: VarDeclStmt, iterable: Expr) extends Stmt
case class ForEachStmt(iterator: VarDeclStmt, iterable: Expr, body: Stmt) extends Stmt

case class BreakStmt(label: BbIndex) extends Stmt

case class ContinueStmt(label: BbIndex) extends Stmt

case class Catch(ty: Type, name: String, body: BlockStmt)

case class TryCatchFinallyStmt(
    body: BlockStmt,
    catches: Seq[Catch],
    finallyBlock: Option[BlockStmt],
) extends Stmt
