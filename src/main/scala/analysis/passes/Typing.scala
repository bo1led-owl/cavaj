package cavaj
package analysis
package passes

import ir.*
import scala.collection.mutable.HashMap

private def getClassOrInterface(
    name: String
)(using
    pkg: IrPackage
): Option[WithQualifiers & WithMethods[IrMethod] & WithFields] =
  (pkg.classes orElse pkg.interfaces).lift(name)

private def getFieldTypeFromHierarcy(
    c: WithFields,
    field: String,
)(using
    pkg: IrPackage
): Option[Type] = {
  val (fields, parents: IterableOnce[String]) = c match
    case Class(_, _, fields, _, implements, extendsClass) => fields -> (implements ++ extendsClass)
    case Interface(_, _, fields, _, implements)           => fields -> implements

  lazy val definedInParent: Option[Field] =
    parents.iterator
      .map(pkg.classes orElse pkg.interfaces)
      .collectFirst { _.fields(field) }

  (fields.get(field) orElse definedInParent) map { _.ty }
}

private def infer(curClass: String, value: Value)(using pkg: IrPackage): Unit =
  value match
    case t: ThisRef => t.ty = Type.Reference(curClass)
    case v          => infer(v)

private def infer(value: Value)(using pkg: IrPackage): Unit =
  value match
    case i: Instr => infer(i)

private def infer(instr: Instr)(using pkg: IrPackage): Unit =
  instr match
    case Load(dest, value)             => infer(value); dest.ty = value.ty
    case Negate(value)                 => infer(value)
    case Not(value)                    => infer(value)
    case b: BinaryInstr                => infer(b.lhs); infer(b.rhs)
    case NewArray(_, _)                => ()
    case ArrayLength(arr)              => infer(arr)
    case ArrayLoad(arr, index)         => infer(arr); infer(index)
    case ArrayStore(arr, index, value) => infer(arr); infer(index); infer(value)
    case Throw(exc)                    => infer(exc)
    case New(_, args)                  => args foreach infer
    case InstanceOf(obj, _)            => infer(obj)
    case g: GetField => {
      infer(g.obj)
      val objClassName = g.obj.ty.asInstanceOf[Type.Reference].name
      g.ty = getClassOrInterface(objClassName).flatMap { getFieldTypeFromHierarcy(_, g.field) }.get
    }
    case g: GetStaticField       => g.ty = getClassOrInterface(g.c).map { _.fields(g.field).ty }.get
    case PutField(obj, _, value) => infer(obj); infer(value)
    case PutStaticField(_, _, value)           => infer(value)
    case InvokeStaticMethod(_, _, args, _)     => args foreach infer
    case InvokeInstanceMethod(obj, _, args, _) => infer(obj); args foreach infer
    case c: CastInstr                          => infer(c.value)
    case Return(value)                         => infer(value)
    case VoidReturn                            => ()
    case Br(cond, _, _)                        => infer(cond)
    case Goto(_)                               => ???

class Typing extends PackagePass[IrMethod] {
  override def apply(pkg: IrPackage): IrPackage = {
    for {
      c           <- pkg.classes.valuesIterator
      overloadSet <- c.methods.valuesIterator
      overload    <- overloadSet
      body        <- overload.body
      bb          <- body
      instr       <- bb.instrs
    } do infer(instr)(using pkg)

    pkg
  }
}
