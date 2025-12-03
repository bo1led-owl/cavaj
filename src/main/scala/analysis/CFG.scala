package cavaj
package analysis

import ir.*

import scala.collection.IndexedSeq
import scala.collection.Seq

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class CfgNode(
    val bb: BbIndex,
    val edges: ArrayBuffer[CfgNode] = ArrayBuffer(),
    val preds: ArrayBuffer[CfgNode] = ArrayBuffer(),
)

private def buildCfg(body: IrMethodBody): (CfgNode, HashSet[CfgNode]) = {
  val nodes = body.bbs.indices.map { CfgNode(_, ArrayBuffer(), ArrayBuffer()) }

  body.bbs
    .map { _.terminator.edges }
    .zipWithIndex
    .foreach { (edges, i) => nodes(i).edges ++= edges.map(nodes) }

  for
    node <- nodes
    curEdges = node.edges
    dest <- curEdges
  do dest.preds += node

  nodes(body.entry) -> HashSet.from(nodes)
}

object CFG {
  def from(body: IrMethodBody): CFG = {
    val (entry, nodes) = buildCfg(body)
    CFG(entry, nodes)
  }
}

class CFG(
    val entry: CfgNode,
    val nodes: HashSet[CfgNode],
) {
  lazy val postOrder: ArrayBuffer[CfgNode] = {
    val visited = HashSet[CfgNode]()
    val res     = ArrayBuffer[CfgNode]()

    def dfs(v: CfgNode): Unit =
      visited += v
      v.edges.iterator filterNot visited foreach dfs
      res += v

    dfs(entry)
    res
  }

  lazy val domTree: DomTree = DomTree(this)

  lazy val dominators: HashMap[CfgNode, HashSet[CfgNode]] = {
    val dom = HashMap.from(nodes.iterator.map { n => n -> nodes })

    val reversedPostOrder = postOrder.reverse

    var changed = true
    while changed do
      changed = false
      for n <- reversedPostOrder do {
        val newSet =
          if n == entry
          then HashSet(n)
          else n.preds.iterator.map(dom).reduce { _ & _ } + n
        if newSet != dom(n) then {
          dom(n) = newSet
          changed = true
        }
      }

    dom
  }

  lazy val backEdges: HashMap[CfgNode, HashSet[CfgNode]] = HashMap.from(
    nodes.iterator
      .map { n => n -> HashSet.from(n.edges.iterator.filter(dominators(n))) }
      .filter { (_, s) => s.nonEmpty }
  )

  def isBackEdge(from: CfgNode, to: CfgNode): Boolean =
    backEdges.lift(from).map { _.contains(to) }.getOrElse(false)
}
