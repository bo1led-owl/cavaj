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

extension (instr: Instr) {
  def toStmt: Stmt =
    instr match
      case VoidReturn    => VoidReturnStmt
      case Return(value) => ReturnStmt(value)
      case _: TerminatorInstr =>
        throw IllegalStateException(
          "non-return terminators cannot be transformed to statements directly"
        )
      case i => ExprStmt(i)

  def usedVariables: HashSet[Int] = instr match
    case b: BinaryInstr              => b.lhs.usedVariables | b.rhs.usedVariables
    case Load(variable, value)       => value.usedVariables + variable.index
    case Negate(v)                   => v.usedVariables
    case Not(v)                      => v.usedVariables
    case NewArray(_, len)            => len.usedVariables
    case ArrayLength(arr)            => arr.usedVariables
    case ArrayLoad(arr, idx)         => arr.usedVariables | idx.usedVariables
    case ArrayStore(arr, idx, value) => arr.usedVariables | idx.usedVariables | value.usedVariables
    case Throw(v)                    => v.usedVariables
    case New(_, args)                => args.foldLeft(HashSet()) { _ | _.usedVariables }
    case InstanceOf(obj, _)          => obj.usedVariables
    case GetField(obj, _, _)         => obj.usedVariables
    case GetStaticField(_, _, _)     => HashSet()
    case PutField(obj, _, value)     => obj.usedVariables | value.usedVariables
    case PutStaticField(_, _, value) => value.usedVariables
    case InvokeStaticMethod(_, _, args, _) => args.foldLeft(HashSet()) { _ | _.usedVariables }
    case InvokeInstanceMethod(obj, _, args, _) =>
      obj.usedVariables | args.foldLeft(HashSet()) { _ | _.usedVariables }
    case CastInstr(_, v) => v.usedVariables
    case Return(v)       => v.usedVariables
    case VoidReturn      => HashSet()
    case Br(cond, _, _)  => cond.usedVariables
    case Goto(_)         => HashSet()
    case Push(_) | Pop(_) =>
      throw IllegalStateException(
        "stack ops must be eliminated before control flow restoration pass"
      )

  // we assume that `Load` cannot be a subexpression
  // because I don't want to write that huge match again
  def assignedVariable: Option[Variable] = instr match
    case Load(variable, _) => Some(variable)
    case _                 => None

  def inlineVars(using values: HashMap[Int, Value]): Instr = instr match
    case Load(variable, value)       => Load(variable, value.inlineVars)
    case Negate(v)                   => Negate(v.inlineVars)
    case Not(v)                      => Not(v.inlineVars)
    case Add(lhs, rhs)               => Add(lhs.inlineVars, rhs.inlineVars)
    case Sub(lhs, rhs)               => Sub(lhs.inlineVars, rhs.inlineVars)
    case Mul(lhs, rhs)               => Mul(lhs.inlineVars, rhs.inlineVars)
    case Div(lhs, rhs)               => Div(lhs.inlineVars, rhs.inlineVars)
    case Rem(lhs, rhs)               => Rem(lhs.inlineVars, rhs.inlineVars)
    case And(lhs, rhs)               => And(lhs.inlineVars, rhs.inlineVars)
    case Or(lhs, rhs)                => Or(lhs.inlineVars, rhs.inlineVars)
    case BitAnd(lhs, rhs)            => BitAnd(lhs.inlineVars, rhs.inlineVars)
    case BitOr(lhs, rhs)             => BitOr(lhs.inlineVars, rhs.inlineVars)
    case Xor(lhs, rhs)               => Xor(lhs.inlineVars, rhs.inlineVars)
    case Shl(lhs, rhs)               => Shl(lhs.inlineVars, rhs.inlineVars)
    case Shr(lhs, rhs)               => Shr(lhs.inlineVars, rhs.inlineVars)
    case UShr(lhs, rhs)              => UShr(lhs.inlineVars, rhs.inlineVars)
    case CmpEq(lhs, rhs)             => CmpEq(lhs.inlineVars, rhs.inlineVars)
    case CmpNe(lhs, rhs)             => CmpNe(lhs.inlineVars, rhs.inlineVars)
    case CmpLt(lhs, rhs)             => CmpLt(lhs.inlineVars, rhs.inlineVars)
    case CmpGt(lhs, rhs)             => CmpGt(lhs.inlineVars, rhs.inlineVars)
    case CmpLe(lhs, rhs)             => CmpLe(lhs.inlineVars, rhs.inlineVars)
    case CmpGe(lhs, rhs)             => CmpGe(lhs.inlineVars, rhs.inlineVars)
    case NewArray(ty, len)           => NewArray(ty, len.inlineVars)
    case ArrayLength(arr)            => ArrayLength(arr.inlineVars)
    case ArrayLoad(arr, idx)         => ArrayLoad(arr.inlineVars, idx.inlineVars)
    case ArrayStore(arr, idx, value) => ArrayStore(arr.inlineVars, idx.inlineVars, value.inlineVars)
    case Throw(v)                    => Throw(v.inlineVars)
    case New(c, args)                => New(c, args.map { _.inlineVars })
    case InstanceOf(obj, c)          => InstanceOf(obj.inlineVars, c)
    case GetField(obj, field, ty)    => GetField(obj.inlineVars, field, ty)
    case g: GetStaticField           => g
    case PutField(obj, field, value) => PutField(obj.inlineVars, field, value.inlineVars)
    case PutStaticField(c, field, value) => PutStaticField(c, field, value.inlineVars)
    case InvokeStaticMethod(c, m, args, ty) =>
      InvokeStaticMethod(c, m, args.map { _.inlineVars }, ty)
    case InvokeInstanceMethod(obj, m, args, ty) =>
      InvokeInstanceMethod(obj.inlineVars, m, args.map { _.inlineVars }, ty)
    case CastInstr(ty, v)          => CastInstr(ty, v.inlineVars)
    case Return(v)                 => Return(v.inlineVars)
    case VoidReturn                => VoidReturn
    case Br(cond, onTrue, onFalse) => Br(cond.inlineVars, onTrue, onFalse)
    case Goto(_)                   => instr
    case Push(_) | Pop(_) =>
      throw IllegalStateException(
        "stack ops must be eliminated before control flow restoration pass"
      )
}

extension (v: Value) {
  def usedVariables: HashSet[Int] = v match
    case i: Instr    => i.usedVariables
    case v: Variable => HashSet(v.index)
    case l: Literal  => HashSet()

  def inlineVars(using values: HashMap[Int, Value]): Value = v match
    case i: Instr    => i.inlineVars
    case v: Variable => values.getOrElse(v.index, v)
    case _           => v
}

private class RestoreControlFlowImpl(cfg: CFG, bbs: ArrayBuffer[BB]) {
  private val loops    = findLoops(cfg)
  private val branches = findBranches(cfg)
  private val cfgNodeByBb: HashMap[BbIndex, CfgNode] =
    HashMap.from(cfg.nodes.iterator.map { n => n.bb -> n })

  private lazy val varDeclaredIn: HashMap[Int, CfgNode] = {
    val res = HashMap[Int, CfgNode]()

    for
      node <- cfg.nodes
      bb = bbs(node.bb)
      instr       <- bb
      assignedVar <- instr.assignedVariable
      varIdx = assignedVar.index
    do res.updateWith(varIdx) { _.filter(cfg.dominators(node)) orElse Some(node) }

    res
  }

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
            case VoidReturn | Return(_) => true
            case _: TerminatorInstr     => false
            case _                      => true
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

  /** Inline all temporary variables to get an expression suitable for loop condition. */
  private def loopCondBbToExpr(idx: BbIndex): Expr = {
    val bb             = bbs(idx)
    val Br(cond, _, _) = bb.terminator.asInstanceOf[Br]

    val temps = varDeclaredIn.filter { (v, n) => n.bb == idx }.keySet

    val values = HashMap[Int, Value]()
    for i <- bb.takeWhile { !_.isTerminator } do
      i match
        case Load(dest, value) => values(dest.index) = value
        case _                 => ()

    var res = cond
    while res.usedVariables intersects temps do res.inlineVars(using values)
    res
  }

  private def restoreLoop(loop: Loop): (Stmt, IterableOnce[CfgNode]) = {
    if loop.exits contains loop.header then {
      // while loop

      val cond = loopCondBbToExpr(loop.header.bb)

      val start = {
        val Br(c, onTrue, onFalse) = bbs(loop.header.bb).terminator.asInstanceOf[Br]
        (loop.body.find { _.bb == onTrue } orElse loop.body.find { _.bb == onFalse })
      }.get

      val body = processSet(loop.body - loop.header, start)

      WhileStmt(loop.header.bb, cond, body) -> (HashSet.from(loop.header.edges) -- loop.body)
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
