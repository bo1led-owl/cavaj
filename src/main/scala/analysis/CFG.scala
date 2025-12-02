package cavaj
package analysis

import ir.*

import scala.collection.IndexedSeq
import scala.collection.Set
import scala.collection.Map
import scala.collection.Seq

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class CfgNode(
    val bb: BbIndex,
    val edges: ArrayBuffer[CfgNode] = ArrayBuffer(),
    val preds: ArrayBuffer[CfgNode] = ArrayBuffer(),
)

private def buildCfg(body: IrMethodBody): (CfgNode, Set[CfgNode]) = {
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

  nodes(body.entry) -> nodes.toSet
}

object CFG {
  def from(body: IrMethodBody): CFG = {
    val (entry, nodes) = buildCfg(body)
    CFG(entry, nodes)
  }
}

class CFG(
    val entry: CfgNode,
    val nodes: Set[CfgNode],
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

  lazy val dominators: Map[CfgNode, Set[CfgNode]] = {
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

  lazy val backEdges: Map[CfgNode, Set[CfgNode]] =
    nodes.iterator
      .map { n => n -> n.edges.iterator.filter(dominators(n)).toSet }
      .filter { (_, s) => s.nonEmpty }
      .toMap
}
