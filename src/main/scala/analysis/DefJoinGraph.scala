package cavaj
package analysis

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

class DefJoinGraph(
    val entry: CfgNode,
    val strongEdges: HashMap[CfgNode, HashSet[CfgNode]],
    val weakEdges: HashMap[CfgNode, HashSet[CfgNode]],
) extends PartialFunction[CfgNode, HashSet[CfgNode]] {
  def this(cfg: CFG) = this(
    cfg.entry,
    cfg.domTree.edges,
    HashMap.from(cfg.nodes.iterator.map { node =>
      val weakEdges = HashSet.from(node.edges) -- cfg.domTree.lift(node).getOrElse(Set())
      node -> weakEdges
    }),
  )

  override def apply(node: CfgNode): HashSet[CfgNode] =
    strongEdges.getOrElse(node, HashSet()) | weakEdges.getOrElse(node, HashSet())

  override def isDefinedAt(x: CfgNode): Boolean =
    strongEdges.isDefinedAt(x) || weakEdges.isDefinedAt(x)
}
