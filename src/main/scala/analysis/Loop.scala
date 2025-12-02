package cavaj
package analysis

import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap

import ir.BbIndex
import scala.collection.mutable.Stack

case class Loop(
    header: CfgNode,
    body: HashSet[CfgNode],
    latches: HashSet[CfgNode],
    exits: HashSet[CfgNode],
) {
  def populateExits: Unit = exits ++= body.filter { _.edges.exists { !body(_) } }
}

def findLoops(cfg: CFG): HashMap[CfgNode, Loop] = {
  // https://pages.cs.wisc.edu/~fischer/cs701.f14/finding.loops.html

  val res = HashMap[CfgNode, Loop]()

  for
    (from, toSet) <- cfg.backEdges
    to            <- toSet
  do {
    val body  = HashSet[CfgNode](to)
    val worklist = Stack[CfgNode](from)

    while worklist.nonEmpty do {
      val n = worklist.pop
      if !body(n) then {
        body += n
        worklist.pushAll(n.preds)
      }
    }

    res.updateWith(to) {
      case None => Some(Loop(to, body, HashSet(from), HashSet()))
      case Some(loop) =>
        Some({
          loop.body ++= body
          loop.latches += from
          loop
        })
    }
  }

  res.valuesIterator.foreach { _.populateExits }
  res
}
