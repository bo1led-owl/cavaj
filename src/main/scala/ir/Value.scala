package cavaj
package ir

trait Value

sealed trait Literal extends Value

case object NullLit extends Literal:
  override def toString: String = "null"

case class ByteLit(value: Byte) extends Literal:
  override def toString: String = value.toString

case class ShortLit(value: Short) extends Literal:
  override def toString: String = value.toString

case class IntLit(value: Int) extends Literal:
  override def toString: String = value.toString

case class LongLit(value: Long) extends Literal:
  override def toString: String = value.toString

case class CharLit(value: Char) extends Literal:
  override def toString: String = value.toString

case class BooleanLit(value: Boolean) extends Literal:
  override def toString: String = value.toString

case class FloatLit(value: Float) extends Literal:
  override def toString: String = value.toString

case class DoubleLit(value: Double) extends Literal:
  override def toString: String = value.toString

case class StringLit(value: String) extends Literal:
  override def toString: String = "\"" + value + "\""

case object ThisRef extends Literal:
  override def toString: String = "this"

case class Variable(index: Int) extends Value:
  override def toString: String = s"v$index"
