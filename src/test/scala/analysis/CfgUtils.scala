package cavaj
package analysis

import ir.BbIndex

def cfgFromIndices(
    desc: (IterableOnce[BbIndex], IterableOnce[BbIndex])*
): (CFG, IndexedSeq[CfgNode]) = {
  val nodes = desc.indices.map { CfgNode(_) }

  for ((edges, preds), i) <- desc.zipWithIndex do
    nodes(i).edges ++= edges.map(nodes)
    nodes(i).preds ++= preds.map(nodes)

  CFG(nodes(0), nodes.toSet) -> nodes
}
