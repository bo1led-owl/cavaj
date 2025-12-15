package cavaj
package analysis

import ir.BbIndex

import scala.collection.mutable.HashSet

def cfgFromIndices(desc: IterableOnce[BbIndex]*): (CFG, IndexedSeq[CfgNode]) = {
  val nodes = desc.indices.map { CfgNode(_) }

  for (edges, i) <- desc.zipWithIndex do nodes(i).edges ++= edges.map(nodes)

  for
    i <- nodes
    j <- i.edges
  do j.preds += i

  CFG(nodes(0), HashSet.from(nodes)) -> nodes
}
