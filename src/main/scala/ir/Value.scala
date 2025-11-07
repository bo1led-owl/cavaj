package cavaj
package ir

trait Value

sealed trait Literal extends Value

case object NullLit                   extends Literal
case class ByteLit(value: Byte)       extends Literal
case class ShortLit(value: Short)     extends Literal
case class IntLit(value: Int)         extends Literal
case class LongLit(value: Long)       extends Literal
case class CharLit(value: Char)       extends Literal
case class BooleanLit(value: Boolean) extends Literal
case class FloatLit(value: Float)     extends Literal
case class DoubleLit(value: Double)   extends Literal
case class StringLit(value: String)   extends Literal
case object ThisRef                   extends Literal

case class Variable(index: Int) extends Value
