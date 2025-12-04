package cavaj
package analysis

import scala.collection.Map
import scala.collection.Set

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.Queue

import scala.util.boundary
import scala.util.boundary.break

class DomTree(val root: CfgNode, val edges: HashMap[CfgNode, HashSet[CfgNode]])
    extends (CfgNode => HashSet[CfgNode]) {
  def this(cfg: CFG) = this(cfg.entry, buildDomTree(cfg.dominators))

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

private def closestDom(
    start: CfgNode,
    isDom: CfgNode => Boolean,
    getPreds: CfgNode => ArrayBuffer[CfgNode] = _.preds,
): Option[CfgNode] = {
  val queue   = Queue(start)
  val visited = HashSet(start)

  boundary:
    while queue.nonEmpty do {
      val v = queue.dequeue
      if isDom(v) then break(Some(v))

      getPreds(v).iterator
        .filterNot(visited)
        .foreach { w =>
          visited += w
          queue.enqueue(w)
        }
    }
    None
}

private def buildDomTree(
    dominators: Map[CfgNode, Set[CfgNode]],
    getPreds: CfgNode => ArrayBuffer[CfgNode] = _.preds,
): HashMap[CfgNode, HashSet[CfgNode]] = {
  val res = HashMap[CfgNode, HashSet[CfgNode]]()

  for
    (node, doms) <- dominators
    strictDoms = doms - node
    if strictDoms.nonEmpty
  do {
    val parent =
      if strictDoms.size == 1
      then strictDoms.head
      else closestDom(node, strictDoms, getPreds).get

    res.updateWith(parent) {
      case None      => Some(HashSet(node))
      case Some(set) => Some(set += node)
    }
  }

  res
}

object PostDomTree {
  def from(cfg: CFG): PostDomTree = {
    val exit = CfgNode(-1, ArrayBuffer(), ArrayBuffer.from(cfg.exits))

    val nodes = cfg.nodes + exit

    val postOrder: ArrayBuffer[CfgNode] = {
      val visited = HashSet[CfgNode]()
      val res     = ArrayBuffer[CfgNode]()

      def dfs(v: CfgNode): Unit =
        visited += v
        v.preds.iterator filterNot visited foreach dfs
        res += v

      dfs(exit)
      res
    }

    def getEdges(node: CfgNode): ArrayBuffer[CfgNode] =
      if cfg.exits(node) then node.edges.clone += exit
      else node.edges

    val dominators: HashMap[CfgNode, HashSet[CfgNode]] = {
      val dom = HashMap.from(nodes.iterator.map { n => n -> nodes })

      val reversedPostOrder = postOrder.reverse

      var changed = true
      while changed do
        changed = false
        for n <- reversedPostOrder do {
          val newSet =
            if n == exit
            then HashSet(n)
            else getEdges(n).iterator.map(dom).reduce { _ & _ } + n
          if newSet != dom(n) then {
            dom(n) = newSet
            changed = true
          }
        }

      dom
    }

    PostDomTree(exit, buildDomTree(dominators, getEdges))
  }
}

class PostDomTree(val exit: CfgNode, val postEdges: HashMap[CfgNode, HashSet[CfgNode]])
    extends DomTree(exit, postEdges) {
  def isExit(n: CfgNode): Boolean = exit == n
}
