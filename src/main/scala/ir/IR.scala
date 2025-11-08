package cavaj
package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap

import scala.collection.IndexedSeq
import scala.collection.Seq

type Qualifiers = Iterable[String]

sealed trait WithQualifiers:
  def qualifiers: Qualifiers

case class Class[M](
    qualifiers: Qualifiers,
    name: String,
    staticFields: HashMap[String, Type],
    fields: HashMap[String, Type],
    staticMethods: HashMap[String, Seq[M]],
    methods: HashMap[String, Seq[M]],
    implements: Seq[String],
    extendsClass: Option[String],
) extends WithQualifiers

case class Method[B](
    qualifiers: Qualifiers,
    name: String,
    parameters: LinkedHashMap[String, Type],
    rettype: Type,
    body: Option[B],
) extends WithQualifiers

type IrMethod = Method[IndexedSeq[BB]]

type BbIndex = Int
case class BB(instrs: IndexedSeq[Instr])
