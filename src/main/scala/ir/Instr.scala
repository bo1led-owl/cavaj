package cavaj
package ir

import scala.collection.Seq

type BbIndex = Int

sealed trait Instr extends Value

sealed trait InstrWithResult extends Instr {
  def dest: Variable
}

case class LoadLiteral(dest: Variable, lit: Literal) extends InstrWithResult

case class Negate(dest: Variable, value: Value) extends InstrWithResult

case class Add(dest: Variable, lhs: Value, rhs: Value)  extends InstrWithResult
case class Sub(dest: Variable, lhs: Value, rhs: Value)  extends InstrWithResult
case class Mul(dest: Variable, lhs: Value, rhs: Value)  extends InstrWithResult
case class Div(dest: Variable, lhs: Value, rhs: Value)  extends InstrWithResult
case class Rem(dest: Variable, lhs: Value, rhs: Value)  extends InstrWithResult
case class And(dest: Variable, lhs: Value, rhs: Value)  extends InstrWithResult
case class Or(dest: Variable, lhs: Value, rhs: Value)   extends InstrWithResult
case class Xor(dest: Variable, lhs: Value, rhs: Value)  extends InstrWithResult
case class Shl(dest: Variable, lhs: Value, rhs: Value)  extends InstrWithResult
case class Shr(dest: Variable, lhs: Value, rhs: Value)  extends InstrWithResult
case class UShr(dest: Variable, lhs: Value, rhs: Value) extends InstrWithResult

case class CmpEq(dest: Variable, lhs: Value, rhs: Value) extends InstrWithResult
case class CmpNe(dest: Variable, lhs: Value, rhs: Value) extends InstrWithResult
case class CmpLt(dest: Variable, lhs: Value, rhs: Value) extends InstrWithResult
case class CmpGt(dest: Variable, lhs: Value, rhs: Value) extends InstrWithResult
case class CmpLe(dest: Variable, lhs: Value, rhs: Value) extends InstrWithResult
case class CmpGe(dest: Variable, lhs: Value, rhs: Value) extends InstrWithResult

// TODO: `multianewarray` (I could not find a way to make the compiler emit it)

case class NewArray(dest: Variable, elemType: Type, len: Value) extends InstrWithResult
case class ArrayLength(dest: Variable, arr: Value)              extends InstrWithResult
case class ArrayLoad(dest: Variable, arr: Value, index: Value)  extends InstrWithResult
case class ArrayStore(arr: Value, index: Value, value: Value)   extends Instr

case class Throw(value: Value) extends Instr

case class New(dest: Variable, c: String, args: Seq[Value]) extends InstrWithResult

// case class Checkcast(obj: Value, target: String)  extends Instr
case class InstanceOf(dest: Variable, obj: Value, target: String) extends InstrWithResult

case class GetField(dest: Variable, obj: Value, field: String)      extends InstrWithResult
case class GetStaticField(dest: Variable, c: String, field: String) extends InstrWithResult

case class PutField(obj: Value, field: String, value: Value)      extends Instr
case class PutStaticField(c: String, field: String, value: Value) extends Instr

// TODO: `invokedynamic` is a strange thing, idk what to do with it

case class InvokeStaticMethod(
    dest: Variable,
    c: String,
    method: String,
    args: Seq[Value],
) extends InstrWithResult

case class InvokeInstanceMethod(
    dest: Variable,
    obj: Value,
    method: String,
    args: Seq[Value],
) extends InstrWithResult

case class InvokeVoidStaticMethod(c: String, method: String, args: Seq[Value])    extends Instr
case class InvokeVoidInstanceMethod(obj: Value, method: String, args: Seq[Value]) extends Instr

case class ToByte(dest: Variable, value: Value)   extends InstrWithResult
case class ToShort(dest: Variable, value: Value)  extends InstrWithResult
case class ToInt(dest: Variable, value: Value)    extends InstrWithResult
case class ToLong(dest: Variable, value: Value)   extends InstrWithResult
case class ToFloat(dest: Variable, value: Value)  extends InstrWithResult
case class ToDouble(dest: Variable, value: Value) extends InstrWithResult

sealed trait TerminatorInstr extends Instr

case class Return(value: Value) extends TerminatorInstr
case object VoidReturn          extends TerminatorInstr

case class Br(cond: Value, onTrue: BbIndex, onFalse: BbIndex) extends TerminatorInstr
case class Goto(target: BbIndex)                              extends TerminatorInstr

// TODO: switch

// TODO: monitorenter, monitorexit (do we really need it?)
// these probably are just the boundaries of a `syncronized` block
