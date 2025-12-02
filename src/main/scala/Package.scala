package cavaj

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.LinkedHashMap

import scala.collection.IndexedSeq
import scala.collection.Seq
import scala.collection.Map

import cavaj.ir.{BB, Value}

case class Package[M](interfaces: Map[String, Interface[M]], classes: Map[String, Class[M]])

enum Qualifier {
  case Public
  case Protected
  case Private
  case Final
  case Abstract
  case Default
  case Static

  override def toString: String = this match
    case Public    => "public"
    case Protected => "protected"
    case Private   => "private"
    case Final     => "final"
    case Abstract  => "abstract"
    case Default   => ""
    case Static    => "static"
}

type Qualifiers = Iterable[Qualifier]

sealed trait WithQualifiers:
  def qualifiers: Qualifiers

sealed trait WithMethods[M]:
  def methods: Map[String, Seq[M]]

sealed trait WithFields:
  def fields: Map[String, Field]

sealed trait ClassLike[M] extends WithQualifiers with WithMethods[M] with WithFields

case class Interface[M](
    qualifiers: Qualifiers,
    name: String,
    fields: Map[String, Field],
    methods: Map[String, Seq[M]],
    implements: Seq[String],
) extends ClassLike[M] {
  override def toString: String = ???
}

case class Class[M](
    qualifiers: Qualifiers,
    name: String,
    fields: Map[String, Field],
    methods: Map[String, Seq[M]],
    implements: Seq[String],
    extendsClass: Option[String],
) extends ClassLike[M] {
  override def toString: String = {
    val quals = qualifiers.filter(_ != Qualifier.Default).mkString("", " ", " ")
    val extendss =
      extendsClass.filterNot(_ == "java.lang.Object").map(e => s" extends $e").getOrElse("")
    val implementss =
      if implements.nonEmpty then s" implements ${implements.mkString(", ")}" else ""

    val fieldss  = fields.values.map("  " + _.toString + ";").mkString("\n")
    val methodss = methods.values.flatten.map(_.toString).mkString("\n\n")

    s"${quals}class $name$extendss$implementss {\n$fieldss\n\n$methodss\n}"
  }
}

case class Field(
    qualifiers: Qualifiers,
    name: String,
    ty: Type,
    value: Option[Value],
) extends WithQualifiers {
  override def toString: String =
    s"${qualifiers.filter(_ != Qualifier.Default).mkString("", " ", " ")}$ty $name"
      + value.map { " = " + _.toString }.getOrElse("")
}

case class Method[B](
    qualifiers: Qualifiers,
    name: String,
    parameters: LinkedHashMap[String, Type],
    rettype: Type,
    body: Option[B],
) extends WithQualifiers {
  override def toString: String = {
    val quals  = qualifiers.filter(_ != Qualifier.Default).mkString("  ", " ", " ")
    val params = parameters.map { case (name, ty) => s"$ty $name" }.mkString(", ")

    val bodys = body match {
      case Some(b: IndexedSeq[?]) if b.forall(_.isInstanceOf[BB]) =>
        val bbs = b.asInstanceOf[IndexedSeq[BB]]
        " {\n" + bbs.zipWithIndex.map { case (bb, i) => s"    BB$i:\n$bb" }.mkString("\n") + "\n  }"
      case _ =>
    }

    val finalName = if name == "<init>" then {
      val className = rettype match {
        case Type.Reference(n) => n.split('.').last
        case _                 => name
      }
      className
    } else name

    val finalRetType = if name == "<init>" then "" else s"$rettype "

    s"$quals$finalRetType$finalName($params)$bodys"
  }
  final def replaceBody[B2](newBody: Option[B2]): Method[B2] =
    Method[B2](qualifiers, name, parameters, rettype, newBody)
}
