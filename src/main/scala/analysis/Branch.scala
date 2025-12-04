package cavaj
package analysis

import scala.collection.Set

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

import scala.util.boundary
import scala.util.boundary.break

extension [A](s: Set[A]) infix def intersects[B >: A](r: Set[A]): Boolean = s.exists(r)

case class Branch(
    header: CfgNode,
    branches: OrderInvariantPair[HashSet[CfgNode]],
    meetingPoint: Option[CfgNode],
)

private def collectBranch(cfg: CFG, meetingPoint: CfgNode)(from: CfgNode): HashSet[CfgNode] = {
  val res = HashSet[CfgNode]()

  val worklist = Stack(from)
  boundary:
    while worklist.nonEmpty do {
      val cur = worklist.pop
      if !res.contains(cur) && cur != meetingPoint then {
        res += cur
        worklist.pushAll(cur.edges.iterator.filter { !cfg.backEdges.contains(_) })
      }
    }

  res
}

def findBranches(cfg: CFG): HashMap[CfgNode, Branch] = {
  val res = HashMap[CfgNode, Branch]()

  val postdomTree = PostDomTree.from(cfg)

  for
    node <- cfg.nodes
    dominates = cfg.domTree(node)
    if dominates.size >= 2
  do {
    assert(dominates.size <= 3)

    val branchHeads = node.edges.filterNot(cfg.isBackEdge(node))

    res(node) = if dominates.size == 3 then {
      // body on both branches

      val mp = (dominates -- branchHeads).head

      Branch(
        node,
        OrderInvariantPair.from(branchHeads.map(collectBranch(cfg, mp))),
        Some(mp),
      )
    } else if dominates.size == 2 &&
      !postdomTree.isExit(postdomTree.lowestCommonAncestor(branchHeads(0), branchHeads(1)))
    then {
      // body on both branches, but meeting point is not dominated

      val mp = postdomTree.lowestCommonAncestor(branchHeads(0), branchHeads(1))

      if postdomTree.isExit(mp) then {
        Branch(
          node,
          OrderInvariantPair.from(branchHeads.map(cfg.reachable)),
          None,
        )
      } else {
        Branch(
          node,
          OrderInvariantPair.from(branchHeads.map(collectBranch(cfg, mp))),
          Some(mp),
        )
      }
    } else if dominates.size == 2 && cfg.reachable(branchHeads(0)).contains(branchHeads(1))
    then {
      // no body on `branchHeads(1)`

      Branch(
        node,
        OrderInvariantPair(collectBranch(cfg, branchHeads(1))(branchHeads(0)), HashSet()),
        Some(branchHeads(1)),
      )
    } else if dominates.size == 2 && cfg.reachable(branchHeads(1)).contains(branchHeads(0))
    then {
      // no body on `branchHeads(0)`

      Branch(
        node,
        OrderInvariantPair(collectBranch(cfg, branchHeads(0))(branchHeads(1)), HashSet()),
        Some(branchHeads(0)),
      )
    } else {
      // no meeting point

      assert(dominates.size == 2)
      Branch(
        node,
        OrderInvariantPair.from[HashSet[CfgNode]](dominates.view.map { cfg.reachable }),
        None,
      )
    }
  }

  res
}
