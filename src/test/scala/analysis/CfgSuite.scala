package cavaj
package analysis

import ir.*

import scala.collection.IndexedSeq
import scala.collection.Map
import scala.collection.Set
import scala.collection.Seq

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

private def dominatorsFromIndices(
    nodes: IndexedSeq[CfgNode],
    desc: IterableOnce[BbIndex]*
): Map[CfgNode, Set[CfgNode]] =
  desc.iterator.zipWithIndex.map { (set, i) => nodes(i) -> set.map(nodes).iterator.toSet }.toMap

class CfgSuite extends munit.FunSuite {
  test("postorder") {
    /*
      / B > D \
     A    /    F
      \ C > E /
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: 2 :: Nil, // A 0
      3 :: Nil,      // B 1
      3 :: 4 :: Nil, // C 2
      5 :: Nil,      // D 3
      5 :: Nil,      // E 4
      Nil,           // F 5
    )

    val postOrders =
      ArrayBuffer(
        ArrayBuffer(5, 3, 1, 4, 2, 0),
        ArrayBuffer(5, 4, 3, 2, 1, 0),
        ArrayBuffer(5, 3, 4, 2, 1, 0),
      ).map { _.map(nodes) }

    assert(postOrders contains cfg.postOrder)
  }

  test("dominators simple") {
    /*
     /> 3 -> 2
    0        |
     \> 4 -> 1
     */

    val (cfg, nodes) = cfgFromIndices(
      3 :: 4 :: Nil, // 0
      2 :: Nil,      // 1
      1 :: Nil,      // 2
      2 :: Nil,      // 3
      1 :: Nil,      // 4
    )

    val expectedDominators = dominatorsFromIndices(
      nodes,
      0 :: Nil,
      0 :: 1 :: Nil,
      0 :: 2 :: Nil,
      0 :: 3 :: Nil,
      0 :: 4 :: Nil,
    )

    assertEquals(cfg.dominators, expectedDominators)
  }

  test("dominators loopy") {
    val (cfg, nodes) = cfgFromIndices(
      1 :: 2 :: Nil,
      2 :: Nil,
      0 :: 1 :: Nil,
    )

    val expectedDominators = dominatorsFromIndices(
      nodes,
      0 :: Nil,
      0 :: 1 :: Nil,
      0 :: 2 :: Nil,
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

    val (cfg, nodes) = cfgFromIndices(
      1 :: Nil,      // 0
      2 :: 4 :: Nil, // 1
      3 :: Nil,      // 2
      Nil,           // 3
      5 :: 6 :: Nil, // 4
      7 :: Nil,      // 5
      1 :: 7 :: Nil, // 6
      3 :: Nil,      // 7
    )

    val expectedDominators = dominatorsFromIndices(
      nodes,
      0 :: Nil,                // 0
      0 :: 1 :: Nil,           // 1
      0 :: 1 :: 2 :: Nil,      // 2
      0 :: 1 :: 3 :: Nil,      // 3
      0 :: 1 :: 4 :: Nil,      // 4
      0 :: 1 :: 4 :: 5 :: Nil, // 5
      0 :: 1 :: 4 :: 6 :: Nil, // 6
      0 :: 1 :: 4 :: 7 :: Nil, // 7
    )

    assertEquals(cfg.dominators, expectedDominators)
  }

  test("back edges 1") {
    val (cfg, nodes) = cfgFromIndices(
      1 :: 2 :: Nil, // 0
      2 :: Nil,      // 1
      0 :: 1 :: Nil, // 2
    )

    val expectedBackEdges = Map((nodes(2), Set(nodes(0))))
    assertEquals(cfg.backEdges, expectedBackEdges)
  }

  test("back edges 2") {
    val (cfg, nodes) = cfgFromIndices(
      1 :: Nil,      // 0
      2 :: Nil,      // 1
      0 :: 3 :: Nil, // 2
      1 :: 4 :: Nil, // 3
      Nil,           // 4
    )

    val expectedBackEdges =
      Map((2, Set(0)), (3, Set(1)))
        .map { (i, s) => nodes(i) -> s.map(nodes) }
    assertEquals(cfg.backEdges, expectedBackEdges)
  }
}
