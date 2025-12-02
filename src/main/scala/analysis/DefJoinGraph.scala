package cavaj
package analysis

import scala.collection.Set
import scala.collection.Map

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class DefJoinGraph(
    val entry: CfgNode,
    val strongEdges: Map[CfgNode, Set[CfgNode]],
    val weakEdges: Map[CfgNode, Set[CfgNode]],
) extends PartialFunction[CfgNode, Set[CfgNode]] {
  def this(cfg: CFG) = this(
    cfg.entry,
    cfg.domTree.edges,
    HashMap.from(cfg.nodes.iterator.map { node =>
      val weakEdges = HashSet.from(node.edges) -- cfg.domTree.lift(node).getOrElse(Set())
      node -> weakEdges
    }),
  )

  override def apply(node: CfgNode): Set[CfgNode] =
    strongEdges.getOrElse(node, Set()) | weakEdges.getOrElse(node, Set())

  override def isDefinedAt(x: CfgNode): Boolean =
    strongEdges.isDefinedAt(x) || weakEdges.isDefinedAt(x)
}
