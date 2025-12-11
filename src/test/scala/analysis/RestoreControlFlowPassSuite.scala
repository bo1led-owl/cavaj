package cavaj
package analysis

import cavaj.ir.*
import cavaj.ast.*

import scala.collection.mutable.LinkedHashMap
import scala.collection.mutable.ArrayBuffer

private def makeMethod(bbs: BB*): IrMethod =
  Method(Nil, "foo", LinkedHashMap(), Type.Void, Some(IrMethodBody(0, ArrayBuffer(bbs*))))

class RestoreControlFlowPassSuite extends munit.FunSuite {
  test("no branches") {
    val method = makeMethod(
      BB(
        Add(IntLit(42), IntLit(4)),
        Goto(1),
      ),
      BB(
        Sub(IntLit(37), IntLit(3)),
        Goto(2),
      ),
      BB(VoidReturn),
    )

    assertEquals(
      passes.RestoreControlFlow.run(method).body.get,
      ExprStmt(Add(IntLit(42), IntLit(4))) ::
        ExprStmt(Sub(IntLit(37), IntLit(3))) ::
        VoidReturnStmt ::
        Nil,
    )
  }

  test("trivial if-then") {
    val method = makeMethod(
      BB(Br(BooleanLit(true), 1, 2)),
      BB(
        Sub(IntLit(37), IntLit(3)),
        Goto(2),
      ),
      BB(VoidReturn),
    )

    assertEquals(
      passes.RestoreControlFlow.run(method).body.get,
      IfStmt(BooleanLit(true), BlockStmt(ExprStmt(Sub(IntLit(37), IntLit(3))) :: Nil), None) ::
        VoidReturnStmt ::
        Nil,
    )
  }

  test("flipped trivial if-then") {
    val method = makeMethod(
      BB(Br(BooleanLit(true), 2, 1)),
      BB(
        Sub(IntLit(37), IntLit(3)),
        Goto(2),
      ),
      BB(VoidReturn),
    )

    assertEquals(
      passes.RestoreControlFlow.run(method).body.get,
      IfStmt(Not(BooleanLit(true)), BlockStmt(ExprStmt(Sub(IntLit(37), IntLit(3))) :: Nil), None) ::
        VoidReturnStmt ::
        Nil,
    )
  }

  test("trivial if-then-else") {
    val method = makeMethod(
      BB(Br(BooleanLit(true), 1, 2)),
      BB(
        Sub(IntLit(37), IntLit(3)),
        Goto(3),
      ),
      BB(
        Add(IntLit(42), IntLit(4)),
        Goto(3),
      ),
      BB(Negate(IntLit(2)), Goto(4)),
      BB(VoidReturn),
    )

    assertEquals(
      passes.RestoreControlFlow.run(method).body.get,
      IfStmt(
        BooleanLit(true),
        BlockStmt(ExprStmt(Sub(IntLit(37), IntLit(3))) :: Nil),
        Some(BlockStmt(ExprStmt(Add(IntLit(42), IntLit(4))) :: Nil)),
      ) ::
        ExprStmt(Negate(IntLit(2))) ::
        VoidReturnStmt ::
        Nil,
    )
  }

  test("nested branches") {
    val add = Add(IntLit(42), IntLit(17))
    val sub = Sub(IntLit(37), IntLit(14))
    val mul = Mul(FloatLit(3), FloatLit(4))
    val div = Div(DoubleLit(3), DoubleLit(4))

    /*
     0
     |\
     1 \
     |\ \
     2 3 4
     |/ /
     5 /
     |/
     6
     */

    val method = makeMethod(
      BB(Br(BooleanLit(true), 1, 4)),  // 0
      BB(Br(BooleanLit(false), 3, 2)), // 1
      BB(add, Goto(5)),                // 2
      BB(sub, Goto(5)),                // 3
      BB(div, Goto(6)),                // 4
      BB(mul, Goto(6)),                // 5
      BB(VoidReturn),                  // 6
    )

    assertEquals(
      passes.RestoreControlFlow.run(method).body.get,
      IfStmt( // 0
        BooleanLit(true),
        BlockStmt(
          IfStmt( // 1
            BooleanLit(false),
            BlockStmt(ExprStmt(sub) :: Nil),      // 3
            Some(BlockStmt(ExprStmt(add) :: Nil)), // 2
          ) :: ExprStmt(mul)                      // 5
            :: Nil
        ),
        Some(BlockStmt(ExprStmt(div) :: Nil)), // 4
      ) ::
        VoidReturnStmt ::
        Nil,
    )
  }
}
