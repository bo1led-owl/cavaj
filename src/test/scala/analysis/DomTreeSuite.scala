package cavaj
package analysis

import ir.*

import scala.collection.IndexedSeq
import scala.collection.Seq

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class DomTreeSuite extends munit.FunSuite {
  test("domtree construction") {
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

    val expectedDomTree = DomTree(
      nodes(0),
      HashMap(
        0 -> HashSet(1),
        1 -> HashSet(2, 3, 4),
        4 -> HashSet(5, 6, 7),
      ).map { (k, v) => nodes(k) -> v.map(nodes) },
    )

    assertEquals(DomTree(cfg), expectedDomTree)
  }

  test("LCA") {
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

    /* dominator tree:

            0
            |
            1
           /|\
          / | \
         2  |  \
         \  |   \
          \ |    4
           \|  / | \
            3  5 6 7
     */

    val dt = DomTree(cfg)

    assertEquals(dt.lowestCommonAncestor(nodes(0), nodes(0)), nodes(0))
    assertEquals(dt.lowestCommonAncestor(nodes(0), nodes(1)), nodes(0))
    assertEquals(dt.lowestCommonAncestor(nodes(0), nodes(3)), nodes(0))

    assertEquals(dt.lowestCommonAncestor(nodes(5), nodes(6)), nodes(4))
    assertEquals(dt.lowestCommonAncestor(nodes(5), nodes(7)), nodes(4))
    assertEquals(dt.lowestCommonAncestor(nodes(6), nodes(7)), nodes(4))

    assertEquals(dt.lowestCommonAncestor(nodes(3), nodes(4)), nodes(1))
    assertEquals(dt.lowestCommonAncestor(nodes(2), nodes(6)), nodes(1))
    assertEquals(dt.lowestCommonAncestor(nodes(1), nodes(6)), nodes(1))
  }
}
