package cavaj
package analysis

private[analysis] case class OrderInvariantPair[A](first: A, second: A) {
  override def canEqual(that: Any): Boolean =
    that match
      case that: OrderInvariantPair[?] => true
      case _                           => false

  override def equals(that: Any): Boolean =
    that match
      case that: OrderInvariantPair[?] =>
        (that.first == first && that.second == second) ||
        (that.first == second && that.second == first)
      case _ => false
}

private[analysis] object OrderInvariantPair {
  def from[A](items: IterableOnce[A]): OrderInvariantPair[A] = {
    val it     = items.iterator
    val first  = it.next
    val second = it.next
    assert(!it.hasNext)
    OrderInvariantPair(first, second)
  }
}
