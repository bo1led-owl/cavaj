package cavaj
package analysis

import scala.collection.mutable.HashMap

import ir.*

trait Pass[M1, M2] {
  def run(c: Class[M1]): Class[M2]

  final infix def andThen[M3](that: Pass[M2, M3]): Pass[M1, M3] =
    val thisP = this
    new Pass[M1, M3] {
      override def run(c: Class[M1]): Class[M3] = (thisP.run andThen that.run)(c)
    }
}

trait MethodPass[M1, M2] extends Pass[M1, M2] {
  // private final def mapInterface(i: Interface[M1]): Interface[M2] = {
  //   val newMethods = i.methods.mapValues { _.map(this.run) }.toMap
  //   Interface(i.qualifiers, i.name, i.fields, newMethods, i.implements)
  // }

  final override def run(c: Class[M1]): Class[M2] = {
    val newMethods = c.methods.mapValues { _.map(this.run) }.toMap
    Class(c.qualifiers, c.name, c.fields, newMethods, c.implements, c.extendsClass)
  }

  def run(method: M1): M2
}
