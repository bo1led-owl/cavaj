package cavaj
package analysis
package passes

import ir.*
import ast.*

import scala.collection.IndexedSeq
import scala.collection.Seq

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue

object VariableMagic extends MethodPass[AstMethod, AstMethod] {
  override def run(method: AstMethod): AstMethod =
    method.replaceBody(method.body.map { body => VariableMagicImpl(method, body).run })
}

extension (b: Boolean) def toInt: Int = if b then 1 else 0

private def replaceV0WithThisRef(in: Instr): Instr = in match
  case Load(Variable(ty, i), value) =>
    assert(i != 0, "why are you loading into `this`")
    val newTy = if ty.isInstanceOf[Type.Reference] then Type.Undef else ty
    Load(Variable(newTy, i), replaceV0WithThisRef(value))
  case Negate(v)         => Negate(replaceV0WithThisRef(v))
  case Not(v)            => Not(replaceV0WithThisRef(v))
  case Add(lhs, rhs)     => Add(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case Sub(lhs, rhs)     => Sub(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case Mul(lhs, rhs)     => Mul(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case Div(lhs, rhs)     => Div(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case Rem(lhs, rhs)     => Rem(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case And(lhs, rhs)     => And(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case Or(lhs, rhs)      => Or(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case BitAnd(lhs, rhs)  => BitAnd(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case BitOr(lhs, rhs)   => BitOr(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case Xor(lhs, rhs)     => Xor(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case Shl(lhs, rhs)     => Shl(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case Shr(lhs, rhs)     => Shr(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case UShr(lhs, rhs)    => UShr(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case CmpEq(lhs, rhs)   => CmpEq(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case CmpNe(lhs, rhs)   => CmpNe(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case CmpLt(lhs, rhs)   => CmpLt(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case CmpGt(lhs, rhs)   => CmpGt(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case CmpLe(lhs, rhs)   => CmpLe(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case CmpGe(lhs, rhs)   => CmpGe(replaceV0WithThisRef(lhs), replaceV0WithThisRef(rhs))
  case NewArray(et, len) => NewArray(et, replaceV0WithThisRef(len))
  case ArrayLength(arr)  => ArrayLength(replaceV0WithThisRef(arr))
  case ArrayLoad(arr, i) => ArrayLoad(replaceV0WithThisRef(arr), replaceV0WithThisRef(i))
  case ArrayStore(arr, i, v) =>
    ArrayStore(
      replaceV0WithThisRef(arr),
      replaceV0WithThisRef(i),
      replaceV0WithThisRef(v),
    )
  case Throw(v)                 => Throw(replaceV0WithThisRef(v))
  case New(c, args)             => New(c, args.map(replaceV0WithThisRef))
  case InstanceOf(obj, target)  => InstanceOf(replaceV0WithThisRef(obj), target)
  case GetField(obj, field, ty) => GetField(replaceV0WithThisRef(obj), field, ty)
  case gsf: GetStaticField      => gsf
  case PutField(obj, f, v)      => PutField(replaceV0WithThisRef(obj), f, replaceV0WithThisRef(v))
  case PutStaticField(c, f, v)  => PutStaticField(c, f, replaceV0WithThisRef(v))
  case InvokeStaticMethod(c, m, args, ty) =>
    InvokeStaticMethod(c, m, args.map(replaceV0WithThisRef), ty)
  case InvokeInstanceMethod(obj, m, args, ty) =>
    InvokeInstanceMethod(replaceV0WithThisRef(obj), m, args.map(replaceV0WithThisRef), ty)
  case CastInstr(ty, v) => CastInstr(ty, replaceV0WithThisRef(v))
  case Return(v)        => Return(replaceV0WithThisRef(v))
  case VoidReturn       => VoidReturn
  case Br(_, _, _) | Goto(_) =>
    throw IllegalStateException(
      "raw control flow ops must be eliminated before variable magic pass"
    )
  case Push(_) | Pop(_) =>
    throw IllegalStateException(
      "stack ops must be eliminated before variable magic pass"
    )

private def replaceV0WithThisRef(in: Value): Value = in match
  case i: Instr        => replaceV0WithThisRef(i)
  case Variable(ty, 0) => ThisRef(ty)
  case v               => v

private class VariableMagicImpl(m: AstMethod, body: Seq[Stmt]) {
  private val static   = m.qualifiers.iterator.contains(Qualifier.Static)
  private val seenVars = HashSet.from(0 until ((!static).toInt + m.parameters.size))

  private def process(e: ExprStmt): Stmt = {
    process(e.value) match
      case Load(dest, value) if !seenVars(dest.index) =>
        seenVars += dest.index
        VarDeclStmt(dest.ty, s"v${dest.index}", value)
      case r => ExprStmt(r)
  }

  private def process(e: Expr): Expr =
    if static then e else replaceV0WithThisRef(e)

  private def process(s: Stmt): Stmt = s match
    case BlockStmt(stmts) => BlockStmt(stmts.map(process))
    case _: VarDeclStmt   => ??? // these statements are impossible before this pass so who cares
    case e: ExprStmt      => process(e)
    case VoidReturnStmt   => VoidReturnStmt
    case ReturnStmt(e)    => ReturnStmt(process(e))
    case IfStmt(cond, onTrue, onFalse) =>
      IfStmt(process(cond), process(onTrue), onFalse.map(process))
    case WhileStmt(cond, body) => WhileStmt(process(cond), process(body))

  def run: Seq[Stmt] = body.map(process)
}
