package cavaj
package ir

trait Value:
  def ty: Type

sealed trait Literal extends Value

case class NullLit(ty: Type = Type.Reference()) extends Literal:
  override def toString: String = "null"

case class ByteLit(value: Byte) extends Literal:
  override def ty: Type         = Type.Byte
  override def toString: String = value.toString

case class ShortLit(value: Short) extends Literal:
  override def ty: Type         = Type.Short
  override def toString: String = value.toString

case class IntLit(value: Int) extends Literal:
  override def ty: Type         = Type.Int
  override def toString: String = value.toString

case class LongLit(value: Long) extends Literal:
  override def ty: Type         = Type.Long
  override def toString: String = value.toString

case class CharLit(value: Char) extends Literal:
  override def ty: Type         = Type.Char
  override def toString: String = value.toString

case class BooleanLit(value: Boolean) extends Literal:
  override def ty: Type         = Type.Boolean
  override def toString: String = value.toString

case class FloatLit(value: Float) extends Literal:
  override def ty: Type         = Type.Float
  override def toString: String = value.toString

case class DoubleLit(value: Double) extends Literal:
  override def ty: Type         = Type.Double
  override def toString: String = value.toString

case class StringLit(value: String) extends Literal:
  override def ty: Type         = Type.Reference("String")
  override def toString: String = "\"" + value + "\""

case class ThisRef(var ty: Type = Type.Reference()) extends Literal:
  override def toString: String = "this"

case class Variable(var ty: Type, index: Int) extends Value:
  override def toString: String = s"v$index"
