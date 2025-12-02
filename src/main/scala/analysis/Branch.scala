package cavaj
package analysis

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack

import scala.util.boundary
import scala.util.boundary.break

case class OrderInvariantPair[A, B](first: A, second: B) {
  override def canEqual(that: Any): Boolean =
    that match
      case that: OrderInvariantPair[?, ?] => true
      case _                              => false

  override def equals(that: Any): Boolean =
    that match
      case that: OrderInvariantPair[?, ?] =>
        (that.first == first && that.second == second) ||
        (that.first == second && that.second == first)
      case _ => false
}

case class Branch(
    header: CfgNode,
    branches: OrderInvariantPair[HashSet[CfgNode], HashSet[CfgNode]],
    meetingPoint: Option[CfgNode],
)

private def collectBranch(
    cfg: CFG,
    from: CfgNode,
    meetingPoint: Option[CfgNode],
): HashSet[CfgNode] = {
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
  val dj = DefJoinGraph(cfg)

  // map from meeting points to sets of nodes that have a J-edge to them
  val possibleBranches = HashMap[CfgNode, HashSet[CfgNode]]()

  for
    (from, toSet) <- dj.weakEdges
    to            <- toSet
    // filter out back edges
    if !cfg.backEdges.lift(from).map { _.contains(to) }.getOrElse(false)
  do
    possibleBranches.updateWith(to) {
      case None      => Some(HashSet(from))
      case Some(set) => Some(set += from)
    }

  val headers = HashMap.from(
    possibleBranches.iterator.map { (meetingPoint, ends) =>
      val headers = HashSet.from(
        for i <- ends.iterator; j <- ends.iterator yield cfg.domTree.lowestCommonAncestor(i, j)
      )
      meetingPoint -> headers
    }
  )

  // TODO: if one of the paths ends with a return,
  // meaning there's no edge to the meeting point,
  // this branch is not detected or detected incorrectly, not sure yet

  for
    (meetingPoint, headers) <- headers
    header                  <- headers
    // FIXME: these `edges(?)` may fail with `IndexOutOfBoundException`
    br1 = collectBranch(cfg, header.edges(0), Some(meetingPoint))
    br2 = collectBranch(cfg, header.edges(1), Some(meetingPoint))
  yield header -> Branch(header, OrderInvariantPair(br1, br2), Some(meetingPoint))
}
