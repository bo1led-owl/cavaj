package cavaj
package ir

enum Type {
  case Void
  case Boolean
  case Byte
  case Short
  case Int
  case Long
  case Char
  case Float
  case Double
  case Reference
  case Class(name: String)

  override def toString: String = this match
    case Void => "void"
    case Boolean => "boolean"
    case Byte => "byte"
    case Short => "short"
    case Int => "int"
    case Long => "long"
    case Char => "char"
    case Float => "float"
    case Double => "double"
    case Reference => "Object"
    case Class(name) => name
}
