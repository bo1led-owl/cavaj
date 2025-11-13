package cavaj
package analysis

import ir.*

sealed trait Pass[M1, M2] extends (Package[M1] => Package[M2]) {
  override def apply(pkg: Package[M1]): Package[M2]

  final infix def andThen[M3](p: PackagePass[M2, M3]): Pass[M1, M3] =
    this match
      case p: PackagePass => _
      case p: MethodPass  =>
}

trait PackagePass[M1, M2] extends Pass[M1, M2]

trait MethodPass[M1, M2]  extends (M1 => M2) with Pass[M1, M2] {
  override def apply(pkg: Package[M1]): Package[M2] =
    Package(
        pkg.interfaces.mapValues()
    )
}
