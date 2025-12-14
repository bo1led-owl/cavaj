package cavaj
package analysis
package passes

import ir.*

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class StackElimination extends MethodPass[IrMethod, IrMethod] {

  private class StackVarManager(nextIndex: () => Int) {
    private val cache = mutable.Map[Int, Variable]()

    def get(stackSlot: Int): Variable = {
      cache.getOrElseUpdate(
        stackSlot,
        Variable(Type.Undef, nextIndex()),
      )
    }
  }

  override def run(method: IrMethod): IrMethod = {
    method.replaceBody(method.body.map(transformBody))
  }

  private def transformBody(body: IrMethodBody): IrMethodBody = {
    val heights    = computeStackHeights(body)
    var varCounter = findAllVariables(body).map(_.index).maxOption.getOrElse(-1) + 1

    def nextFreeIndex(): Int = {
      val i = varCounter
      varCounter += 1
      i
    }

    val stackVars = StackVarManager(nextFreeIndex)
    val newBlocks = ArrayBuffer[BB]()

    for (bb, bbIdx) <- body.bbs.zipWithIndex do {
      val inputHeight = heights(bbIdx)

      val simStack = mutable.Stack[Value]()
      for i <- 0 until inputHeight do {
        simStack.push(stackVars.get(i))
      }

      val newInstrs = ArrayBuffer[Instr]()

      for instr <- bb do {
        instr match {
          case Push(value) =>
            if isComplex(value) then {
              val temp = Variable(value.ty, nextFreeIndex())
              newInstrs += Load(temp, value)
              simStack.push(temp)
            } else {
              simStack.push(value)
            }

          case Pop(dest) =>
            if simStack.isEmpty then {
              throw RuntimeException(s"stack underflow in block $bbIdx")
            }
            val valToLoad = simStack.pop()
            newInstrs += Load(dest, valToLoad)

          case t: TerminatorInstr =>
            val valuesOnStack = simStack.toSeq.reverse

            for (value, i) <- valuesOnStack.zipWithIndex do {
              val canonicalVar = stackVars.get(i)
              if value != canonicalVar then {
                newInstrs += Load(canonicalVar, value)
              }
            }
            newInstrs += t

          case other => newInstrs += other
        }
      }

      newBlocks += BB(newInstrs)
    }

    body.copy(bbs = newBlocks)
  }

  private def isComplex(v: Value): Boolean = v match {
    case _: Literal | _: Variable => false
    case _                        => true
  }

  private def computeStackHeights(body: IrMethodBody): Array[Int] = {
    val heights = Array.fill(body.bbs.length)(-1)
    val queue   = mutable.Queue[Int]()

    heights(body.entry) = 0
    queue.enqueue(body.entry)

    while queue.nonEmpty do {
      val idx = queue.dequeue()
      val hIn = heights(idx)

      var currentHeight = hIn
      for instr <- body.bbs(idx) do {
        instr match {
          case _: Push => currentHeight += 1
          case _: Pop  => currentHeight -= 1
          case _       =>
        }
      }

      val terminator = body.bbs(idx).terminator
      for succ <- terminator.edges do {
        if heights(succ) == -1 then {
          heights(succ) = currentHeight
          queue.enqueue(succ)
        } else if heights(succ) != currentHeight then {
          throw RuntimeException(
            s"""
            | stack height mismatch at block $succ:
            | expected ${heights(succ)} but got $currentHeight from block $idx
            """
          )
        }
      }
    }
    heights
  }

  private def findAllVariables(body: IrMethodBody): Set[Variable] = {
    val vars = mutable.Set[Variable]()

    def collect(v: Value): Unit = v match {
      case v: Variable => vars += v
      case i: Instr =>
        i match {
          case Push(v)                             => collect(v)
          case Pop(d)                              => collect(d)
          case Load(d, v)                          => collect(d); collect(v)
          case Negate(v)                           => collect(v)
          case Not(v)                              => collect(v)
          case b: BinaryInstr                      => collect(b.lhs); collect(b.rhs)
          case NewArray(_, l)                      => collect(l)
          case ArrayLength(v)                      => collect(v)
          case ArrayLoad(a, i)                     => collect(a); collect(i)
          case ArrayStore(a, i, v)                 => collect(a); collect(i); collect(v)
          case Throw(v)                            => collect(v)
          case New(_, args)                        => args.foreach(collect)
          case InstanceOf(v, _)                    => collect(v)
          case GetField(o, _, _)                   => collect(o)
          case PutField(o, _, v)                   => collect(o); collect(v)
          case PutStaticField(_, _, v)             => collect(v)
          case InvokeStaticMethod(_, _, args, _)   => args.foreach(collect)
          case InvokeInstanceMethod(o, _, args, _) => collect(o); args.foreach(collect)
          case CastInstr(_, v)                     => collect(v)
          case Return(v)                           => collect(v)
          case Br(c, _, _)                         => collect(c)
          case _                                   => ()
        }
      case _ =>
    }

    for bb <- body.bbs; instr <- bb do collect(instr)
    vars.toSet
  }
}
