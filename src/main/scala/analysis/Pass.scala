package cavaj
package analysis

import scala.collection.mutable.HashMap

import ir.*

sealed trait Pass[M1, M2] {
  def run(pkg: Package[M1]): Package[M2]

  final infix def andThen[M3](that: PackagePass[M2, M3]): Pass[M1, M3] =
    val thisP = this
    new Pass[M1, M3] {
      override def run(pkg: Package[M1]): Package[M3] = (thisP.run andThen that.run)(pkg)
    }
}

trait PackagePass[M1, M2] extends Pass[M1, M2]

trait MethodPass[M1, M2] extends Pass[M1, M2] {
  private final def mapInterface(i: Interface[M1]): Interface[M2] = {
    val newMethods = i.methods.mapValues { _.map(this.run) }.toMap
    Interface(i.qualifiers, i.name, i.fields, newMethods, i.implements)
  }

  private final def mapClass(c: Class[M1]): Class[M2] = {
    val newMethods = c.methods.mapValues { _.map(this.run) }.toMap
    Class(c.qualifiers, c.name, c.fields, newMethods, c.implements, c.extendsClass)
  }

  def run(method: M1): M2

  final override def run(pkg: Package[M1]): Package[M2] =
    Package(
      pkg.interfaces.mapValues(mapInterface).toMap,
      pkg.classes.mapValues(mapClass).toMap,
    )
}
