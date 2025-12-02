package cavaj
package analysis
package passes

import ir.*
import ast.*

import scala.collection.Set

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue

extension (instr: Instr)
  def toStmt: Stmt =
    instr match
      case _: TerminatorInstr => ??? // should not be reachable
      case i                  => ExprStmt(i)

private class RestoreControlFlowImpl(cfg: CFG, bbs: ArrayBuffer[BB]) {
  private val loops: HashMap[CfgNode, Loop] = findLoops(cfg)

  def this(body: IrMethodBody) = this(CFG.from(body), body.bbs)

  private def restoreLoop(loop: Loop): (Stmt, CfgNode) = {
    if loop.exits.contains(loop.header) then {
      // while loop
    } else {
      // do-while loop
    }

    ???
  }

  private def processNode(node: CfgNode): (IterableOnce[Stmt], IterableOnce[CfgNode]) = {
    if loops.contains(node) then {
      // loop
      val (stmt, nextNode) = restoreLoop(loops(node))
      (stmt :: Nil, nextNode :: Nil)
    } else if node.edges.size > 1 then {
      // if statement

      ???
    } else {
      val bb = bbs(node.bb)
      (bb.takeWhile { !_.isTerminator }.map { _.toStmt }, node.edges)
    }
  }

  def run: ArrayBuffer[Stmt] = {
    val res = ArrayBuffer[Stmt]()

    val visited = HashSet[CfgNode]()
    val queue   = Queue[CfgNode](cfg.entry)
    while queue.nonEmpty do {
      val node = queue.dequeue
      if !visited(node) then {
        val (stmt, nextNodes) = processNode(node)
        visited += node
        res ++= stmt
        queue.enqueueAll(nextNodes)
      }
    }

    res
  }
}

class RestoreControlFlow extends MethodPass[IrMethod, AstMethod] {
  override def run(method: IrMethod): AstMethod =
    method.replaceBody(
      method.body map { body => RestoreControlFlowImpl(body).run }
    )
}
