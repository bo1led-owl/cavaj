package cavaj
package analysis

import ir.*

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class BranchesSuite extends munit.FunSuite {
  test("simple if-else") {
    /*
      0
     / \
    1   2
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: 2 :: Nil, // 0
      Nil,           // 1
      Nil,           // 2
    )

    val branches = findBranches(cfg)

    val expectedBranches: HashMap[CfgNode, Branch] = HashMap(
      nodes(0) -> Branch(nodes(0), OrderInvariantPair(HashSet(nodes(1)), HashSet(nodes(2))), None)
    )

    assertEquals(branches, expectedBranches)
  }

  test("diamond") {
    /*
      0
     / \
    1   2
     \ /
      3
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: 2 :: Nil, // 0
      3 :: Nil,      // 1
      3 :: Nil,      // 2
      Nil,           // 3
    )

    val branches = findBranches(cfg)

    val expectedBranches: HashMap[CfgNode, Branch] = HashMap(
      nodes(0) -> Branch(
        nodes(0),
        OrderInvariantPair(HashSet(nodes(1)), HashSet(nodes(2))),
        Some(nodes(3)),
      )
    )

    assertEquals(branches, expectedBranches)
  }

  test("simple if + no else") {
    /*
        0
       / \
      1   |
       \ /
        2
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: 2 :: Nil, // 0
      2 :: Nil,      // 1
      Nil,           // 2
    )

    val branches = findBranches(cfg)

    val expectedBranches: HashMap[CfgNode, Branch] = HashMap(
      nodes(0) -> Branch(nodes(0), OrderInvariantPair(HashSet(nodes(1)), HashSet()), Some(nodes(2)))
    )

    assertEquals(branches, expectedBranches)
  }

  test("complex if + no else") {
    /*
        0
       / \
      1   |
      |   |
      2   |
       \ /
        3
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: 3 :: Nil, // 0
      2 :: Nil,      // 1
      3 :: Nil,      // 2
      Nil,           // 3
    )

    val branches = findBranches(cfg)

    val expectedBranches: HashMap[CfgNode, Branch] = HashMap(
      nodes(0) -> Branch(
        nodes(0),
        OrderInvariantPair(HashSet(1, 2).map(nodes), HashSet()),
        Some(nodes(3)),
      )
    )

    assertEquals(branches, expectedBranches)
  }

  test("nested branches") {
    /*
        0
       / \
      /   \
     1     4
    / \   / \
   2   3 5   6
   \   | |   /
    \  | |  /
     \ | | /
      \\ //
        7
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: 4 :: Nil, // 0
      2 :: 3 :: Nil, // 1
      7 :: Nil,      // 2
      7 :: Nil,      // 3
      5 :: 6 :: Nil, // 4
      7 :: Nil,      // 5
      7 :: Nil,      // 6
      Nil,           // 7
    )

    val branches = findBranches(cfg)

    val expectedBranches: HashMap[CfgNode, Branch] = HashMap.from(
      Set(
        Branch(
          nodes(0),
          OrderInvariantPair(HashSet(1, 2, 3).map(nodes), HashSet(4, 5, 6).map(nodes)),
          Some(nodes(7)),
        ),
        Branch(
          nodes(1),
          OrderInvariantPair(HashSet(nodes(2)), HashSet(nodes(3))),
          Some(nodes(7)),
        ),
        Branch(
          nodes(4),
          OrderInvariantPair(HashSet(nodes(5)), HashSet(nodes(6))),
          Some(nodes(7)),
        ),
      ).map { b => b.header -> b }
    )

    assertEquals(branches, expectedBranches)
  }
}
