import cavaj.analysis.{CFG, CfgNode}
import cavaj.ir.*

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

class CfgSuite extends munit.FunSuite {
  test("postorder") {
    /*
      / B > D \
     A    /    F
      \ C > E /
     */

    val bbs = ArrayBuffer(
      BB(Br(NullLit(), 1, 2)), // A 0
      BB(Goto(3)),             // B 1
      BB(Br(NullLit(), 3, 4)), // C 2
      BB(Goto(5)),             // D 3
      BB(Goto(5)),             // E 4
      BB(VoidReturn),          // F 5
    )

    val cfg       = CFG(bbs)
    val postOrder = cfg.postOrder

    val postOrders: IterableOnce[Seq[BbIndex]] =
      List(
        List(5, 3, 1, 4, 2, 0),
        List(5, 4, 3, 2, 1, 0),
        List(5, 3, 4, 2, 1, 0),
      )

    assert(postOrders.iterator contains postOrder)
  }

  test("dominators simple") {
    /*
     /> 3 -> 2
    0        |
     \> 4 -> 1
     */

    val bbs = ArrayBuffer(
      BB(Br(NullLit(), 3, 4)),
      BB(Goto(2)),
      BB(Goto(1)),
      BB(Goto(2)),
      BB(Goto(1)),
    )

    val cfg = CFG(bbs)

    val expectedDominators = ArrayBuffer(
      HashSet(0),
      HashSet(0, 1),
      HashSet(0, 2),
      HashSet(0, 3),
      HashSet(0, 4),
    )

    assertEquals(cfg.dominators, expectedDominators)
  }

  test("dominators deep") {
    /*
            0
            |
            1 <-\
           / \   \
          /   4   \
         /   / \  /
        2   5   6
        \   \  /
         \   7
          \ /
           3
     */

    val bbs = ArrayBuffer(
      BB(Goto(1)),
      BB(Br(NullLit(), 2, 4)),
      BB(Goto(3)),
      BB(VoidReturn),
      BB(Br(NullLit(), 5, 6)),
      BB(Goto(7)),
      BB(Br(NullLit(), 1, 7)),
      BB(Goto(3)),
    )

    val cfg = CFG(bbs)

    val expectedDominators = ArrayBuffer(
      HashSet(0),          // 0
      HashSet(0, 1),       // 1
      HashSet(0, 1, 2),    // 2
      HashSet(0, 1, 3),    // 3
      HashSet(0, 1, 4),    // 4
      HashSet(0, 1, 4, 5), // 5
      HashSet(0, 1, 4, 6), // 6
      HashSet(0, 1, 4, 7), // 7
    )

    assertEquals(cfg.dominators, expectedDominators)
  }
}
