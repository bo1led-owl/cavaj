package cavaj
package analysis

import scala.collection.Set

extension [A, B](t: (A, B))
  private[analysis] def bimap[C, D](f: A => C, g: B => D): (C, D) = (f(t._1), g(t._2))
  private[analysis] infix def mapFirst[C](f: A => C): (C, B)      = (f(t._1), t._2)
  private[analysis] infix def mapSecond[C](f: B => C): (A, C)     = (t._1, f(t._2))

extension [A](s: Set[A])
  private[analysis] infix def intersects[B >: A](r: Set[A]): Boolean = s exists r
