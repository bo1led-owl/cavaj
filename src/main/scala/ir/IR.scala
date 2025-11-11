package cavaj
package ir

type IrClass = Class[IrMethod]
type IrMethod = Method[IndexedSeq[BB]]

type BbIndex = Int
case class BB(instrs: IndexedSeq[Instr])
