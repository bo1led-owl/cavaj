package cavaj
package analysis

import ir.*

import scala.collection.IndexedSeq
import scala.collection.Set
import scala.collection.Seq

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet

extension (t: TerminatorInstr)
  private def getEdges: Seq[BbIndex] =
    t match
      case Br(cond, onTrue, onFalse) => onTrue :: onFalse :: Nil
      case Goto(target)              => target :: Nil
      case Return(_) | VoidReturn    => Nil

class CfgNode(val bb: BB, val edges: Seq[BbIndex], val preds: Seq[BbIndex])

private def buildCfg(bbs: IndexedSeq[BB]): ArrayBuffer[CfgNode] = {
  val edges = bbs.map {
    _.find { _.isTerminator }
      .map { _.asInstanceOf[TerminatorInstr].getEdges }
      .get
  }

  val preds = IndexedSeq.fill[ArrayBuffer[BbIndex]](bbs.length)(ArrayBuffer())

  for
    i <- bbs.indices
    curEdges = edges(i)
    dest <- curEdges
  do preds(dest) += i

  val nodes = bbs.iterator zip edges.iterator zip preds.iterator map
    { case ((bb, edges), preds) => CfgNode(bb, edges, preds.toSeq) }

  ArrayBuffer.from(nodes)
}

class CFG(nodes: ArrayBuffer[CfgNode]) extends IndexedSeq[CfgNode] {
  def this(bbs: IndexedSeq[BB]) = this(buildCfg(bbs))
  def this(bbs: BB*) = this(ArrayBuffer(bbs*))

  override def apply(i: Int): CfgNode = repr(i)
  override def length: Int            = repr.length

  lazy val postOrder: ArrayBuffer[BbIndex] = {
    val visited = HashSet[BbIndex]()
    val res     = ArrayBuffer[BbIndex]()

    def dfs(v: BbIndex): Unit =
      visited += v
      nodes(v).edges.iterator filterNot visited foreach dfs
      res += v

    dfs(0)
    res
  }

  lazy val dominators: IndexedSeq[Set[BbIndex]] = {
    def allNodes = HashSet.from(nodes.indices)

    val dom = ArrayBuffer.fill[HashSet[BbIndex]](nodes.length)(allNodes)

    val reversedPostOrder = postOrder.reverse

    var changed = true
    while changed do
      changed = false
      for n <- reversedPostOrder do {
        val newSet =
          if n == 0 // replace this with ` == entry`
          then HashSet(n)
          else nodes(n).preds.iterator.map(dom).reduce { _ intersect _ } + n
        if newSet != dom(n) then {
          dom(n) = newSet
          changed = true
        }
      }

    dom
  }

  lazy val backEdges: Seq[(BbIndex, BbIndex)] =
    nodes.zipWithIndex
      .flatMap { (n, i) => n.edges.map { i -> _ } }
      .filter { (i, j) => dominators(i)(j) }
}
