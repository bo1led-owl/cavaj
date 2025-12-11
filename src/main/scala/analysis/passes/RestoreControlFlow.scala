package cavaj
package analysis
package passes

import ir.*
import ast.*

import scala.collection.IndexedSeq

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.Queue

object RestoreControlFlow extends MethodPass[IrMethod, AstMethod] {
  override def run(method: IrMethod): AstMethod =
    method.mapBody({ RestoreControlFlowImpl(_).run })
}

extension [A, B](t: (A, B))
  def bimap[C, D](f: A => C, g: B => D): (C, D) = (f(t._1), g(t._2))
  infix def mapFirst[C](f: A => C): (C, B)      = (f(t._1), t._2)
  infix def mapSecond[C](f: B => C): (A, C)     = (t._1, f(t._2))

extension (instr: Instr)
  def toStmt: Stmt =
    instr match
      case VoidReturn         => VoidReturnStmt
      case Return(value)      => ReturnStmt(value)
      case _: TerminatorInstr => ??? // should not be reachable
      case i                  => ExprStmt(i)

private class RestoreControlFlowImpl(cfg: CFG, bbs: ArrayBuffer[BB]) {
  private val loops    = findLoops(cfg)
  private val branches = findBranches(cfg)
  private val cfgNodeByBb: HashMap[BbIndex, CfgNode] =
    HashMap.from(cfg.nodes.iterator.map { n => n.bb -> n })

  private val loopStack    = Stack[(Loop, Int)]()
  private val labelCounter = Iterator.from(0)

  def this(body: IrMethodBody) = this(CFG.from(body), body.bbs)

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

  private def nodeToStmts(node: CfgNode): ArrayBuffer[Stmt] =
    ArrayBuffer.from(
      bbs(node.bb)
        .takeWhile {
          _ match
            case VoidReturn | Return => true
            case _: TerminatorInstr  => false
            case _                   => true
        }
        .map { _.toStmt }
    )

  private def processNode(node: CfgNode): (IterableOnce[Stmt], IterableOnce[CfgNode]) = {
    if loops.contains(node) then restoreLoop(loops(node)) mapFirst { _ :: Nil }
    else if branches.contains(node) then restoreBranch(branches(node))
    else if node.edges.size > 1 then {
      // if statement with a continue/break as one of the branches
      ???
    } else nodeToStmts(node) -> node.edges
  }

  private def processSet(nodes: HashSet[CfgNode], start: CfgNode): Stmt = {
    val queue   = Queue(start)
    val visited = HashSet[CfgNode]()
    val res     = ArrayBuffer[Stmt]()
    while queue.nonEmpty do {
      val node = queue.dequeue
      if !visited(node) then {
        val (stmt, nextNodes) = processNode(node)
        visited += node
        res ++= stmt
        queue.enqueueAll(nextNodes.iterator.filter(nodes))
      }
    }

    BlockStmt(res)
  }

  private def restoreLoop(loop: Loop): (Stmt, IterableOnce[CfgNode]) = {
    if loop.exits.contains(loop.header) then {
      // while loop

      ???
    } else {
      // do-while loop
      assert(loop.exits intersects loop.latches)

      ???
    }
  }

  private def restoreBranch(b: Branch): (IterableOnce[Stmt], IterableOnce[CfgNode]) = {
    val headerStmts = nodeToStmts(b.header)

    def findStart(nodes: HashSet[CfgNode]): CfgNode =
      nodes.find { _.preds.forall { !nodes(_) } }.get

    val Br(cond, onTrue, onFalse) = bbs(b.header.bb).terminator.asInstanceOf[Br]

    val nonEmptyBranch: Option[HashSet[CfgNode]] =
      Option.when(b.branches.first.isEmpty)(b.branches.second) orElse
        Option.when(b.branches.second.isEmpty)(b.branches.first)

    val stmt = nonEmptyBranch match
      case None => {
        // if-then-else

        val firstStart  = findStart(b.branches.first)
        val secondStart = findStart(b.branches.second)

        assert(
          (firstStart.bb == onTrue && secondStart.bb == onFalse) ||
            (firstStart.bb == onFalse && secondStart.bb == onTrue)
        )
        val (onTrueNodes, onFalseNodes) =
          if firstStart.bb == onTrue
          then b.branches.first  -> b.branches.second
          else b.branches.second -> b.branches.first

        IfStmt(
          cond,
          processSet(onTrueNodes, findStart(onTrueNodes)),
          Some(processSet(onFalseNodes, findStart(onFalseNodes))),
        )
      }
      case Some(nodes) => {
        // if-then
        val start = findStart(nodes)
        assert(start.bb == onTrue || start.bb == onFalse)
        val pathOnTrue = start.bb == onTrue
        IfStmt(if pathOnTrue then cond else Not(cond), processSet(nodes, start), None)
      }

    (headerStmts += stmt) -> b.meetingPoint
  }
}
