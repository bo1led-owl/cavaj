package cavaj
package analysis

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue

import scala.util.boundary
import scala.util.boundary.break

class DomTree(val root: CfgNode, val edges: HashMap[CfgNode, HashSet[CfgNode]])
    extends PartialFunction[CfgNode, HashSet[CfgNode]] {
  def this(cfg: CFG) = this(cfg.entry, buildDomTree(cfg))

  private lazy val lcaDepths: ArrayBuffer[(CfgNode, Int)] = {
    val visited = HashSet[CfgNode]()
    val res     = ArrayBuffer[(CfgNode, Int)]()

    def dfs(u: CfgNode, depth: Int = 0): Unit = {
      visited += u
      res += u -> depth
      if !this.isDefinedAt(u) then return
      for v <- this(u).iterator.filterNot(visited) do {
        dfs(v, depth + 1)
        res += u -> depth
      }
    }

    dfs(root)
    res
  }

  def lowestCommonAncestor(i: CfgNode, j: CfgNode): CfgNode = {
    val iIdx = lcaDepths.indexWhere { (node, _) => node == i }
    val jIdx = lcaDepths.indexWhere { (node, _) => node == j }
    lcaDepths.slice(iIdx min jIdx, (iIdx max jIdx) + 1).minBy { _._2 }._1
  }

  override def apply(node: CfgNode): HashSet[CfgNode] = edges(node)
  override def isDefinedAt(x: CfgNode): Boolean       = edges.isDefinedAt(x)

  override def equals(that: Any): Boolean =
    that match
      case that: DomTree => root == that.root && edges == that.edges
      case _             => false

  override def toString: String = s"DomTree($root, $edges)"
}

private def closestDom(start: CfgNode, isDom: CfgNode => Boolean): Option[CfgNode] = {
  val queue   = Queue(start)
  val visited = HashSet(start)

  boundary:
    while queue.nonEmpty do {
      val v = queue.dequeue
      if isDom(v) then break(Some(v))

      v.preds.iterator.filterNot(visited).foreach { w =>
        visited += w
        queue.enqueue(w)
      }
    }
    None
}

private def buildDomTree(cfg: CFG): HashMap[CfgNode, HashSet[CfgNode]] = {
  val res = HashMap[CfgNode, HashSet[CfgNode]]()

  for
    (node, doms) <- cfg.dominators
    strictDoms = doms - node
    if strictDoms.nonEmpty
  do {
    val parent =
      if strictDoms.size == 1
      then strictDoms.head
      else closestDom(node, strictDoms).get

    res.updateWith(parent) {
      case None      => Some(HashSet(node))
      case Some(set) => Some(set += node)
    }
  }

  res
}
