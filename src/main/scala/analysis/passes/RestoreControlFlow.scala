package cavaj
package analysis
package passes

import ir.*
import ast.*

import scala.collection.Seq
import scala.collection.IndexedSeq
import scala.collection.mutable.ArrayBuffer

private def restoreControlFlow(body: ArrayBuffer[BB]): ArrayBuffer[Stmt] = {
  val cfg = CFG(body)

  //   method.body map { body =>
  //     for {
  //       bb    <- body
  //       instr <- bb
  //     } yield {
  //       instr match
  //         case t: TerminatorInstr => ???
  //         case i                  => ExprStmt(i)
  //     }
  //   }

  ???
}

class RestoreControlFlow extends MethodPass[IrMethod, AstMethod] {
  override def run(method: IrMethod): AstMethod =
    method.replaceBody(method.body map restoreControlFlow)
}
