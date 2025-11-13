package cavaj
package analysis

import scala.collection.mutable.HashMap

import ir.*

sealed trait Pass[M1, M2] extends (Package[M1] => Package[M2]) {
  override def apply(pkg: Package[M1]): Package[M2]

  final infix def andThen[M3](p: PackagePass[M2, M3]): Pass[M1, M3] =
    val thisP = this
    new Pass[M1, M3] {
      override def apply(pkg: Package[M1]): Package[M3] = (thisP andThen p)(pkg)
    }
}

trait PackagePass[M1, M2] extends Pass[M1, M2]

trait MethodPass[M1, M2] extends (M1 => M2) with Pass[M1, M2] {
  private def mapInterface(i: Interface[M1]): Interface[M2] = {
    val newMethods = i.methods.mapValues { _.map(this.apply) }.toMap
    Interface(i.qualifiers, i.name, i.fields, newMethods, i.implements)
  }

  private def mapClass(c: Class[M1]): Class[M2] = {
    val newMethods = c.methods.mapValues { _.map(this.apply) }.toMap
    Class(c.qualifiers, c.name, c.fields, newMethods, c.implements, c.extendsClass)
  }

  override def apply(pkg: Package[M1]): Package[M2] =
    Package(
      pkg.interfaces.mapValues(mapInterface).toMap,
      pkg.classes.mapValues(mapClass).toMap,
    )
}
