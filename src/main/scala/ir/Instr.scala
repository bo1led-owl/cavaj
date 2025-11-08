package cavaj
package ir

import scala.collection.Seq

sealed trait Instr extends Value

case class Load(dest: Variable, value: Value) extends Instr:
  override def toString: String = s"$dest = $value"

case class Negate(value: Value) extends Instr:
  override def toString: String = s"-($value)"

case class Add(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) + ($rhs)"

case class Sub(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) - ($rhs)"

case class Mul(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) * ($rhs)"

case class Div(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) / ($rhs)"

case class Rem(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) % ($rhs)"

case class And(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) & ($rhs)"

case class Or(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) | ($rhs)"

case class Xor(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) ^ ($rhs)"

case class Shl(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) << ($rhs)"

case class Shr(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) >> ($rhs)"

case class UShr(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) >>> ($rhs)"

case class CmpEq(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) == ($rhs)"

case class CmpNe(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) != ($rhs)"

case class CmpLt(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) < ($rhs)"

case class CmpGt(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) > ($rhs)"

case class CmpLe(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) <= ($rhs)"

case class CmpGe(lhs: Value, rhs: Value) extends Instr:
  override def toString: String = s"($lhs) >= ($rhs)"

// TODO: `multianewarray` (I could not find a way to make the compiler emit it)

case class NewArray(elemType: Type, len: Value) extends Instr:
  override def toString: String = s"new $elemType[$len]"

case class ArrayLength(arr: Value) extends Instr:
  override def toString: String = s"($arr).length"

case class ArrayLoad(arr: Value, index: Value) extends Instr:
  override def toString: String = s"($arr)[$index]"

case class ArrayStore(arr: Value, index: Value, value: Value) extends Instr:
  override def toString: String = s"($arr)[$index] = $value"

case class Throw(value: Value) extends Instr:
  override def toString: String = s"throw $value"

case class New(c: String, args: Seq[Value]) extends Instr:
  override def toString: String = s"new $c(${args.mkString(", ")})"

// case class Checkcast(obj: Value, target: String)  extends Instr:
// override def toString: String = ???
case class InstanceOf(obj: Value, target: String) extends Instr:
  override def toString: String = s"($obj) instanceof $target"

case class GetField(obj: Value, field: String) extends Instr:
  override def toString: String = s"($obj).$field"

case class GetStaticField(c: String, field: String) extends Instr:
  override def toString: String = s"$c.$field"

case class PutField(obj: Value, field: String, value: Value) extends Instr:
  override def toString: String = s"($obj).$field = $value"

case class PutStaticField(c: String, field: String, value: Value) extends Instr:
  override def toString: String = s"$c.$field = $value"

// TODO: `invokedynamic` is a strange thing, idk what to do with it

case class InvokeStaticMethod(c: String, method: String, args: Seq[Value]) extends Instr:
  override def toString: String = s"$c.$method(${args.mkString(", ")})"

case class InvokeInstanceMethod(obj: Value, method: String, args: Seq[Value]) extends Instr:
  override def toString: String = s"($obj).$method(${args.mkString(", ")})"

case class ToByte(value: Value) extends Instr:
  override def toString: String = s"(byte)($value)"

case class ToShort(value: Value) extends Instr:
  override def toString: String = s"(short)($value)"

case class ToInt(value: Value) extends Instr:
  override def toString: String = s"(int)($value)"

case class ToLong(value: Value) extends Instr:
  override def toString: String = s"(long)($value)"

case class ToFloat(value: Value) extends Instr:
  override def toString: String = s"(float)($value)"

case class ToDouble(value: Value) extends Instr:
  override def toString: String = s"(double)($value)"

sealed trait TerminatorInstr extends Instr

case class Return(value: Value) extends TerminatorInstr:
  override def toString: String = s"return $value"

case object VoidReturn extends TerminatorInstr:
  override def toString: String = "return"

case class Br(cond: Value, onTrue: BbIndex, onFalse: BbIndex) extends TerminatorInstr
case class Goto(target: BbIndex)                              extends TerminatorInstr

// TODO: switch

// TODO: monitorenter, monitorexit (do we really need it?)
// these probably are just the boundaries of a `syncronized` block
