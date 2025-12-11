package cavaj
package analysis

import cavaj.ir.*

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ArrayBuffer

class RestoreControlFlowPassSuite extends munit.FunSuite {
  test("no branches") {
    val method = Method[IrMethodBody](
      Nil,
      "noBranches",
      LinkedHashMap(),
      Type.Void,
      Some(
        IrMethodBody(
          0,
          ArrayBuffer(
            BB(
              Add(IntLit(42), IntLit(4)),
              Goto(1),
            ),
            BB(
              Sub(IntLit(37), IntLit(3)),
              Goto(2),
            ),
            BB(VoidReturn),
          ),
        )
      ),
    )

    val res = passes.RestoreControlFlow.run(method)
    println(res)
  }
}
