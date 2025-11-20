package cavaj
package ir

import scala.collection.Seq

sealed trait Instr extends Value {
  def isTerminator: Boolean = false
}

case class Load(dest: Variable, value: Value) extends Instr:
  override def ty: Type         = Type.Void
  override def toString: String = s"$dest = $value"

case class Negate(value: Value) extends Instr:
  override def ty: Type         = value.ty
  override def toString: String = s"-($value)"

case class Not(value: Value) extends Instr:
  override def ty: Type         = value.ty
  override def toString: String = s"!($value)"

sealed trait BinaryInstr(op: String) extends Instr:
  def lhs: Value
  def rhs: Value
  override def toString: String = s"($lhs) $op ($rhs)"
  override def ty: Type =
    assert(lhs.ty == rhs.ty)
    lhs.ty

case class Add(lhs: Value, rhs: Value)    extends BinaryInstr("+")
case class Sub(lhs: Value, rhs: Value)    extends BinaryInstr("-")
case class Mul(lhs: Value, rhs: Value)    extends BinaryInstr("*")
case class Div(lhs: Value, rhs: Value)    extends BinaryInstr("/")
case class Rem(lhs: Value, rhs: Value)    extends BinaryInstr("%")
case class And(lhs: Value, rhs: Value)    extends BinaryInstr("&&")
case class Or(lhs: Value, rhs: Value)     extends BinaryInstr("||")
case class BitAnd(lhs: Value, rhs: Value) extends BinaryInstr("&")
case class BitOr(lhs: Value, rhs: Value)  extends BinaryInstr("|")
case class Xor(lhs: Value, rhs: Value)    extends BinaryInstr("^")
case class Shl(lhs: Value, rhs: Value)    extends BinaryInstr("<<")
case class Shr(lhs: Value, rhs: Value)    extends BinaryInstr(">>")
case class UShr(lhs: Value, rhs: Value)   extends BinaryInstr(">>>")

sealed trait CmpInstr extends BinaryInstr:
  override def ty: Type = Type.Boolean

case class CmpEq(lhs: Value, rhs: Value) extends CmpInstr with BinaryInstr("==")
case class CmpNe(lhs: Value, rhs: Value) extends CmpInstr with BinaryInstr("!=")
case class CmpLt(lhs: Value, rhs: Value) extends CmpInstr with BinaryInstr("<")
case class CmpGt(lhs: Value, rhs: Value) extends CmpInstr with BinaryInstr(">")
case class CmpLe(lhs: Value, rhs: Value) extends CmpInstr with BinaryInstr("<=")
case class CmpGe(lhs: Value, rhs: Value) extends CmpInstr with BinaryInstr(">=")

// TODO: `multianewarray` (I could not find a way to make the compiler emit it)

case class NewArray(elemType: Type, len: Value) extends Instr:
  override def ty: Type         = Type.Array(elemType)
  override def toString: String = s"new $elemType[$len]"

case class ArrayLength(arr: Value) extends Instr:
  override def ty: Type         = Type.Int
  override def toString: String = s"($arr).length"

case class ArrayLoad(arr: Value, index: Value) extends Instr:
  override def ty: Type         = arr.ty.asInstanceOf[Type.Array].elemType
  override def toString: String = s"($arr)[$index]"

case class ArrayStore(arr: Value, index: Value, value: Value) extends Instr:
  override def ty: Type         = Type.Void
  override def toString: String = s"($arr)[$index] = $value"

case class Throw(value: Value) extends Instr:
  override def ty: Type         = Type.Void
  override def toString: String = s"throw $value"

case class New(c: String, args: Seq[Value]) extends Instr:
  override def ty: Type         = Type.Reference(c)
  override def toString: String = s"new $c(${args.mkString(", ")})"

// case class Checkcast(obj: Value, target: String)  extends Instr:

case class InstanceOf(obj: Value, target: String) extends Instr:
  override def ty: Type         = Type.Boolean
  override def toString: String = s"($obj) instanceof $target"

case class GetField(obj: Value, field: String, var ty: Type = Type.Undef) extends Instr:
  override def toString: String = s"($obj).$field"

case class GetStaticField(c: String, field: String, var ty: Type = Type.Undef) extends Instr:
  override def toString: String = s"$c.$field"

case class PutField(obj: Value, field: String, value: Value) extends Instr:
  override def ty: Type         = Type.Void
  override def toString: String = s"($obj).$field = $value"

case class PutStaticField(c: String, field: String, value: Value) extends Instr:
  override def ty: Type         = Type.Void
  override def toString: String = s"$c.$field = $value"

// TODO: `invokedynamic` is a strange thing, idk what to do with it

case class InvokeStaticMethod(
    c: String,
    method: String,
    args: Seq[Value],
    ty: Type,
) extends Instr:
  override def toString: String = s"$c.$method(${args.mkString(", ")})"

case class InvokeInstanceMethod(
    obj: Value,
    method: String,
    args: Seq[Value],
    ty: Type,
) extends Instr:
  override def toString: String = s"($obj).$method(${args.mkString(", ")})"

case class CastInstr(ty: Type, value: Value) extends Instr:
  override def toString: String = s"($ty)($value)"

sealed trait TerminatorInstr extends Instr {
  override def ty: Type              = Type.Void
  override def isTerminator: Boolean = true
}

case class Return(value: Value) extends TerminatorInstr:
  override def toString: String = s"return $value"

case object VoidReturn extends TerminatorInstr:
  override def toString: String = "return"

case class Br(cond: Value, onTrue: BbIndex, onFalse: BbIndex) extends TerminatorInstr
case class Goto(target: BbIndex)                              extends TerminatorInstr

// TODO: switch

// TODO: monitorenter, monitorexit (do we really need it?)
// these probably are just the boundaries of a `syncronized` block
