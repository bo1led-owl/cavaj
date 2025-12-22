package cavaj

enum Type {
  case Undef
  case Void
  case Boolean
  case Byte
  case Short
  case Int
  case Long
  case Char
  case Float
  case Double
  case Reference(name: String = "Object")
  case Array(elemType: Type)

  override def toString: String = this match
    case Undef           => "var"
    case Void            => "void"
    case Boolean         => "boolean"
    case Byte            => "byte"
    case Short           => "short"
    case Int             => "int"
    case Long            => "long"
    case Char            => "char"
    case Float           => "float"
    case Double          => "double"
    case Array(elemType) => s"$elemType[]"
    case Reference(name) => name
}
