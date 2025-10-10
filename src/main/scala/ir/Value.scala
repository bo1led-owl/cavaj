package cavaj
package ir

trait Value

case object NullLit                   extends Value
case class ByteLit(value: Byte)       extends Value
case class ShortLit(value: Short)     extends Value
case class IntLit(value: Int)         extends Value
case class LongLit(value: Long)       extends Value
case class CharLit(value: Char)       extends Value
case class BooleanLit(value: Boolean) extends Value
case class FloatLit(value: Float)     extends Value
case class DoubleLit(value: Double)   extends Value

case object ThisRef             extends Value
case class Variable(index: Int) extends Value
