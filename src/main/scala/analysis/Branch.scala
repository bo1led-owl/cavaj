package cavaj
package analysis

import scala.collection.Set

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

case class Branch(
    header: CfgNode,
    branches: OrderInvariantPair[HashSet[CfgNode]],
    meetingPoint: Option[CfgNode],
)

private def collectBranch(
    cfg: CFG,
    meetingPoint: Option[CfgNode],
)(
    from: CfgNode
): HashSet[CfgNode] = {
  val res = HashSet[CfgNode]()

  val worklist = Stack(from)
  while worklist.nonEmpty do {
    val cur = worklist.pop
    if !res.contains(cur) && meetingPoint.filter { _ == cur }.isEmpty then {
      res += cur
      worklist.pushAll(cur.forwardEdges(cfg))
    }
  }
  res
}

def findBranches(cfg: CFG): HashMap[CfgNode, Branch] = {
  val res = HashMap[CfgNode, Branch]()

  for
    node <- cfg.nodes
    branchHeads = node.forwardEdges(cfg).toArray
    if branchHeads.length == 2
  do {
    val mp: Option[CfgNode] = {
      val reachable = cfg.reachableForward(branchHeads(0))
      cfg.bfs(reachable, branchHeads(1), { _.forwardEdges(cfg) })
    }

    res(node) = Branch(
      node,
      OrderInvariantPair.from(branchHeads.map(collectBranch(cfg, mp))),
      mp,
    )
  }

  res
}
