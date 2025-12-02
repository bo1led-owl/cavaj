package cavaj
package analysis

import ir.*

import scala.collection.Map

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class LoopsSuite extends munit.FunSuite {
  test("find nested loops") {
    /*
            0
            |
         /->1--\
        |   |   \
        | />2-\ |
        | | | | |
        | \-3 | |
        |     | |
        \---4</ |
               /
              /
            5
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: Nil,      // 0
      2 :: 5 :: Nil, // 1
      3 :: 4 :: Nil, // 2
      2 :: Nil,      // 3
      1 :: Nil,      // 4
      Nil,           // 5
    )

    val loops = findLoops(cfg)

    val expectedLoops: HashMap[CfgNode, Loop] = HashMap.from(
      Set(
        Loop(nodes(1), HashSet(1, 2, 3, 4).map(nodes), HashSet(nodes(4)), HashSet(nodes(1))),
        Loop(nodes(2), HashSet(2, 3).map(nodes), HashSet(nodes(3)), HashSet(nodes(2))),
      ).map { loop => loop.header -> loop }
    )

    assertEquals(loops, expectedLoops)
  }

  test("loop with `continue`") {
    /*
      />0  --> 5
     |  | \
     |  1  \
     | / \ |
     | 2 3-/
     \ \
      \-4
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: 5 :: Nil, // 0
      2 :: 3 :: Nil, // 1
      4 :: Nil,      // 2
      0 :: Nil,      // 3
      0 :: Nil,      // 4
      Nil,           // 5
    )

    val loops = findLoops(cfg)

    val expectedLoops: HashMap[CfgNode, Loop] = HashMap.from(
      Set(
        Loop(
          nodes(0),
          HashSet(0, 1, 2, 3, 4).map(nodes),
          HashSet(3, 4).map(nodes),
          HashSet(nodes(0)),
        )
      ).map { loop => loop.header -> loop }
    )

    assertEquals(loops, expectedLoops)
  }

  test("loop with `break`") {
    /*
      />0--\
     |  |   \
     |  1    \
     | / \    \
     | 2 3-\   |
     \ \ /  \  |
      \-4    \ /
              5
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: 5 :: Nil, // 0
      2 :: 3 :: Nil, // 1
      4 :: Nil,      // 2
      4 :: 5 :: Nil, // 3
      0 :: Nil,      // 4
      Nil,           // 5
    )

    val loops = findLoops(cfg)

    val expectedLoops: HashMap[CfgNode, Loop] = HashMap.from(
      Set(
        Loop(
          nodes(0),
          HashSet(0, 1, 2, 3, 4).map(nodes),
          HashSet(4).map(nodes),
          HashSet(0, 3).map(nodes),
        )
      ).map { loop => loop.header -> loop }
    )

    assertEquals(loops, expectedLoops)
  }

  test("do while") {
    /*
      0
      |
    />1
    | |
    | 2
    \ |
     \3
      |
      4
     */

    val (cfg, nodes) = cfgFromIndices(
      1 :: Nil,      // 0
      2 :: Nil,      // 1
      3 :: Nil,      // 2
      1 :: 4 :: Nil, // 3
      Nil,           // 4
    )

    val loops = findLoops(cfg)

    val expectedLoops: HashMap[CfgNode, Loop] = HashMap.from(
      Set(
        Loop(
          nodes(1),
          HashSet(1, 2, 3).map(nodes),
          HashSet(3).map(nodes),
          HashSet(3).map(nodes),
        )
      ).map { loop => loop.header -> loop }
    )

    assertEquals(loops, expectedLoops)
  }
}
