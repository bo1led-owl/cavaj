package cavaj
package analysis

import ir.*

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class ControlFlowDetectionSuite extends munit.FunSuite {
  test("loops are not branches") {
    /*
        0
        |\
        1 |
       /|/
      | 2
       \|
        3
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: Nil,      // 0
      2 :: Nil,      // 1
      0 :: 3 :: Nil, // 2
      1 :: Nil,      // 3
    )

    assert(findBranches(cfg).isEmpty)
    assert(findLoops(cfg).nonEmpty)
  }

  test("branches are not loops") {
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

    assert(findBranches(cfg).nonEmpty)
    assert(findLoops(cfg).isEmpty)
  }

  test("branching in a loop") {
    /*
      />0<\
     /  |  \
    |   1  |
    |  / \ /
    | 2   3
     \ \ /
      \<4
        |
        5
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: Nil,      // 0
      2 :: 3 :: Nil, // 1
      4 :: Nil,      // 2
      0 :: 4 :: Nil, // 3
      0 :: 5 :: Nil, // 4
      Nil,           // 5
    )

    assertEquals(
      findBranches(cfg),
      HashMap(
        nodes(1) ->
          Branch(
            nodes(1),
            OrderInvariantPair(
              HashSet(2).map(nodes),
              HashSet(3).map(nodes),
            ),
            Some(nodes(4)),
          )
      ),
    )

    assertEquals(
      findLoops(cfg),
      HashMap(
        nodes(0) ->
          Loop(
            nodes(0),
            HashSet(0, 1, 2, 3, 4).map(nodes),
            HashSet(3, 4).map(nodes),
            HashSet(4).map(nodes),
          )
      ),
    )
  }

  test("looping in a branch") {
    /*
        0
       / \
      1   4
     /|   |
    | 2   5
     \|   |
      3   6
       \ /
        7
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: 4 :: Nil, // 0
      2 :: Nil,      // 1
      3 :: Nil,      // 2
      1 :: 7 :: Nil, // 3
      5 :: Nil,      // 4
      6 :: Nil,      // 5
      7 :: Nil,      // 6
      Nil,           // 7
    )

    assertEquals(
      findBranches(cfg),
      HashMap(
        nodes(0) ->
          Branch(
            nodes(0),
            OrderInvariantPair(
              HashSet(1, 2, 3).map(nodes),
              HashSet(4, 5, 6).map(nodes),
            ),
            Some(nodes(7)),
          )
      ),
    )

    assertEquals(
      findLoops(cfg),
      HashMap(
        nodes(1) ->
          Loop(
            nodes(1),
            HashSet(1, 2, 3).map(nodes),
            HashSet(3).map(nodes),
            HashSet(3).map(nodes),
          )
      ),
    )
  }

  test("nested everything") {
    // for image check `images/nested_everything_cfg.svg`

    val (cfg, nodes) = cfgFromIndices(
      1 :: 12 :: Nil,  // 0
      2 :: 3 :: Nil,   // 1
      0 :: 8 :: Nil,   // 2
      4 :: Nil,        // 3
      5 :: 6 :: Nil,   // 4
      7 :: Nil,        // 5
      7 :: Nil,        // 6
      3 :: 11 :: Nil,  // 7
      9 :: 10 :: Nil,  // 8
      Nil,             // 9
      11 :: Nil,       // 10
      19 :: Nil,       // 11
      13 :: 14 :: Nil, // 12
      16 :: Nil,       // 13
      15 :: Nil,       // 14
      17 :: Nil,       // 15
      18 :: Nil,       // 16
      12 :: 18 :: Nil, // 17
      12 :: 19 :: Nil, // 18
      0 :: Nil,        // 19
    )

    def makeBranch(
        header: Int,
        l: IterableOnce[Int],
        r: IterableOnce[Int],
        mp: Option[Int],
    ): (CfgNode, Branch) =
      nodes(header) -> Branch(
        nodes(header),
        OrderInvariantPair(
          HashSet.from(l.iterator.map(nodes)),
          HashSet.from(r.iterator.map(nodes)),
        ),
        mp.map(nodes),
      )

    def makeLoop(
        header: Int,
        body: IterableOnce[Int],
        latches: IterableOnce[Int],
        exits: IterableOnce[Int],
    ): (CfgNode, Loop) =
      nodes(header) -> Loop(
        nodes(header),
        HashSet.from(body.iterator.map(nodes)),
        HashSet.from(latches.iterator.map(nodes)),
        HashSet.from(exits.iterator.map(nodes)),
      )

    assertEquals(
      findBranches(cfg),
      HashMap(
        makeBranch(0, 1 to 11, 12 to 18, Some(19)),
        makeBranch(1, 2 :: 8 :: 9 :: 10 :: Nil, 3 to 7, Some(11)),
        makeBranch(4, 5 :: Nil, 6 :: Nil, Some(7)),
        makeBranch(8, 9 :: Nil, 10 :: 11 :: 19 :: Nil, None),
        makeBranch(12, 13 :: 16 :: Nil, 14 :: 15 :: 17 :: Nil, Some(18)),
      ),
    )

    assertEquals(
      findLoops(cfg),
      HashMap(
        makeLoop(0, HashSet.from(0 to 19) -= 9, 2 :: 19 :: Nil, 8 :: Nil),
        makeLoop(3, 3 to 7, 7 :: Nil, 7 :: Nil),
        makeLoop(12, 12 to 18, 17 :: 18 :: Nil, 18 :: Nil),
      ),
    )
  }
}
