package cavaj
package analysis
package passes

import ir.*
import ast.*

import scala.collection.Set

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

private def restoreControlFlow(body: IrMethodBody): ArrayBuffer[Stmt] = {
  val loops: HashMap[CfgNode, Loop] = findLoops(CFG.from(body))

  val res = ArrayBuffer[Stmt]()

  ???

  res
}

class RestoreControlFlow extends MethodPass[IrMethod, AstMethod] {
  override def run(method: IrMethod): AstMethod =
    method.replaceBody(method.body map restoreControlFlow)
}
