package cavaj
package ir

import scala.collection.mutable.ArrayBuffer

type IrPackage   = Package[IrMethod]
type IrInterface = Interface[IrMethod]
type IrClass     = Class[IrMethod]
type IrMethod    = Method[ArrayBuffer[BB]]

type BbIndex = Int

class BB(repr: ArrayBuffer[Instr]) extends IndexedSeq[Instr] {
  assert(repr.exists { _.isTerminator }, "basic block must contain a terminator instruction")

  def this(instrs: IterableOnce[Instr]) =
    this(ArrayBuffer.from(instrs))

  def this(instrs: Instr*) = this(ArrayBuffer(instrs*))

  override def apply(i: BbIndex): Instr = repr(i)
  override def length: Int              = repr.length
}
