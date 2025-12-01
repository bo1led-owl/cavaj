package cavaj
package ir

type IrPackage   = Package[IrMethod]
type IrInterface = Interface[IrMethod]
type IrClass     = Class[IrMethod]
type IrMethod    = Method[IndexedSeq[BB]]

type BbIndex = Int
case class BB(instrs: IndexedSeq[Instr]) {
  override def toString: String = {
    instrs.map("      " + _.toString).mkString("\n")
  }
}
