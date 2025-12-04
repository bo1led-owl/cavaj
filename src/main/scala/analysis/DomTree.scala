package cavaj
package analysis

import scala.collection.Map
import scala.collection.Set

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue

class DomTree(val root: CfgNode, val edges: HashMap[CfgNode, HashSet[CfgNode]])
    extends (CfgNode => HashSet[CfgNode]) {
  def this(cfg: CFG) = this(cfg.entry, buildDomTree(cfg))

  private lazy val lcaDepths: ArrayBuffer[(CfgNode, Int)] = {
    val visited = HashSet[CfgNode]()
    val res     = ArrayBuffer[(CfgNode, Int)]()

    def dfs(u: CfgNode, depth: Int = 0): Unit = {
      visited += u
      res += u -> depth
      if !edges.contains(u) then return
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

  override def apply(node: CfgNode): HashSet[CfgNode] = edges.lift(node).getOrElse(HashSet())

  override def equals(that: Any): Boolean =
    that match
      case that: DomTree => root == that.root && edges == that.edges
      case _             => false

  override def toString: String = s"DomTree($root, $edges)"
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
      else cfg.bfs(strictDoms, node, _.preds).get

    res.updateWith(parent) {
      case None      => Some(HashSet(node))
      case Some(set) => Some(set += node)
    }
  }

  res
}
