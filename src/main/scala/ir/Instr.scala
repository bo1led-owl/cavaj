package cavaj
package ir

import scala.collection.Seq

sealed trait Instr extends Value

case class Load(dest: Variable, value: Value) extends Instr

case class Negate(value: Value) extends Instr

case class Add(lhs: Value, rhs: Value)  extends Instr
case class Sub(lhs: Value, rhs: Value)  extends Instr
case class Mul(lhs: Value, rhs: Value)  extends Instr
case class Div(lhs: Value, rhs: Value)  extends Instr
case class Rem(lhs: Value, rhs: Value)  extends Instr
case class And(lhs: Value, rhs: Value)  extends Instr
case class Or(lhs: Value, rhs: Value)   extends Instr
case class Xor(lhs: Value, rhs: Value)  extends Instr
case class Shl(lhs: Value, rhs: Value)  extends Instr
case class Shr(lhs: Value, rhs: Value)  extends Instr
case class UShr(lhs: Value, rhs: Value) extends Instr

case class CmpEq(lhs: Value, rhs: Value) extends Instr
case class CmpNe(lhs: Value, rhs: Value) extends Instr
case class CmpLt(lhs: Value, rhs: Value) extends Instr
case class CmpGt(lhs: Value, rhs: Value) extends Instr
case class CmpLe(lhs: Value, rhs: Value) extends Instr
case class CmpGe(lhs: Value, rhs: Value) extends Instr

// TODO: `multianewarray` (I could not find a way to make the compiler emit it)

case class NewArray(elemType: Type, len: Value)               extends Instr
case class ArrayLength(arr: Value)                            extends Instr
case class ArrayLoad(arr: Value, index: Value)                extends Instr
case class ArrayStore(arr: Value, index: Value, value: Value) extends Instr

case class Throw(value: Value) extends Instr

case class New(c: String, args: Seq[Value]) extends Instr

// case class Checkcast(obj: Value, target: String)  extends Instr
case class InstanceOf(obj: Value, target: String) extends Instr

case class GetField(obj: Value, field: String)      extends Instr
case class GetStaticField(c: String, field: String) extends Instr

case class PutField(obj: Value, field: String, value: Value)      extends Instr
case class PutStaticField(c: String, field: String, value: Value) extends Instr

// TODO: `invokedynamic` is a strange thing, idk what to do with it

case class InvokeStaticMethod(
    dest: Variable,
    c: String,
    method: String,
    args: Seq[Value],
) extends Instr

case class InvokeInstanceMethod(
    dest: Variable,
    obj: Value,
    method: String,
    args: Seq[Value],
) extends Instr

case class InvokeVoidStaticMethod(c: String, method: String, args: Seq[Value])    extends Instr
case class InvokeVoidInstanceMethod(obj: Value, method: String, args: Seq[Value]) extends Instr

case class ToByte(value: Value)   extends Instr
case class ToShort(value: Value)  extends Instr
case class ToInt(value: Value)    extends Instr
case class ToLong(value: Value)   extends Instr
case class ToFloat(value: Value)  extends Instr
case class ToDouble(value: Value) extends Instr

sealed trait TerminatorInstr extends Instr

case class Return(value: Value) extends TerminatorInstr
case object VoidReturn          extends TerminatorInstr

case class Br(cond: Value, onTrue: BbIndex, onFalse: BbIndex) extends TerminatorInstr
case class Goto(target: BbIndex)                              extends TerminatorInstr

// TODO: switch

// TODO: monitorenter, monitorexit (do we really need it?)
// these probably are just the boundaries of a `syncronized` block
