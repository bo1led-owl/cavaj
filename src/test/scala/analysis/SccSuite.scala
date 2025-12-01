package cavaj
package analysis

import ir.*

import scala.collection.Set

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

// class SccSuite extends munit.FunSuite {
//   test("sccs") {
//     /*
//             0
//             |
//             1 <-\
//            / \   \
//           /   4   \
//          /   / \  /
//         2   5   6
//         \   \  /
//          \   7
//           \ /
//            3
//      */

//     val (cfg, nodes) = cfgFromIndices(
//       (1 :: Nil, Nil),                // 0
//       (2 :: 4 :: Nil, 0 :: 6 :: Nil), // 1
//       (3 :: Nil, 1 :: Nil),           // 2
//       (Nil, 2 :: 7 :: Nil),           // 3
//       (5 :: 6 :: Nil, 1 :: Nil),      // 4
//       (7 :: Nil, 4 :: Nil),           // 5
//       (1 :: 7 :: Nil, 4 :: Nil),      // 6
//       (3 :: Nil, 5 :: 6 :: Nil),      // 7
//     )

//     val sccs = findSccs(cfg)

//     val expectedSccs: Set[Set[CfgNode]] =
//       Set(Set(1, 4, 6), Set(0), Set(2), Set(3), Set(5), Set(7))
//         .map { _.map(nodes) }
//     assertEquals(sccs, expectedSccs)
//   }

//   test("scc in scc") {
//     /*
//             0
//             |
//          /->1--\
//         |   |   \
//         | />2-\ |
//         | | | | |
//         | \-3 | |
//         |     | |
//         \---4</ |
//                /
//               /
//             5
//      */

//     val (cfg, nodes) = cfgFromIndices(
//       (1 :: Nil, Nil),                // 0
//       (2 :: 5 :: Nil, 0 :: 4 :: Nil), // 1
//       (3 :: 4 :: Nil, 1 :: 3 :: Nil), // 2
//       (2 :: Nil, 2 :: Nil),           // 3
//       (1 :: Nil, 1 :: Nil),           // 4
//       (Nil, 1 :: Nil),                // 5
//     )

//     val sccs = findSccs(cfg)

//     val expectedSccs: Set[Set[CfgNode]] =
//       Set(Set(1, 2, 3, 4), Set(0), Set(5))
//         .map { _.map(nodes) }

//     assertEquals(sccs, expectedSccs)
//   }

//   test("do while") {
//     /*
//        />0
//        | |
//        \-1
//          |
//          2
//      */

//     val (cfg, nodes) = cfgFromIndices(
//       (1 :: Nil, 1 :: Nil), // 0
//       (2 :: Nil, 0 :: Nil), // 1
//       (Nil, 1 :: Nil),      // 2
//     )

//     val sccs = findSccs(cfg)

//     val expectedSccs: Set[Set[CfgNode]] =
//       Set(Set(0, 1), Set(2))
//         .map { _.map(nodes) }
//     assertEquals(sccs, expectedSccs)
//   }
// }
