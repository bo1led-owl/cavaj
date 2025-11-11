package cavaj
package ast

import ir.*

import scala.collection.Seq
import cavaj.Type

type AstClass = Class[AstMethod]
type AstMethod = Method[Seq[Stmt]]

type Expr = Value
sealed trait Stmt

case class VarDeclStmt(ty: Type, name: String, value: Expr) extends Stmt

case class ExprStmt(value: Expr) extends Stmt

case class BlockStmt(stmts: Seq[Stmt]) extends Stmt

case class IfStmt(cond: Expr, onTrue: Stmt, onFalse: Option[Stmt]) extends Stmt

case class WhileStmt(cond: Expr, body: Stmt)   extends Stmt
case class DoWhileStmt(cond: Expr, body: Stmt) extends Stmt

case class ForStmt(
    init: VarDeclStmt | Seq[Expr],
    cond: Option[Expr],
    step: Seq[Expr],
    body: Stmt,
) extends Stmt

case class ForEachStmt(iterator: VarDeclStmt, iterable: Expr) extends Stmt

case object BreakStmt extends Stmt

case object ContinueStmt extends Stmt

case class Catch(ty: Type, name: String, body: BlockStmt)

case class TryCatchFinallyStmt(
    body: BlockStmt,
    catches: Seq[Catch],
    finallyBlock: Option[BlockStmt],
) extends Stmt
