package cavaj
package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap

case class Class(
    staticFields: HashMap[String, (Type, Value)],
    fields: HashMap[String, Type],
    staticMethods: HashMap[String, Method],
    methods: HashMap[String, Method],
)

case class Method(
    parameters: ArrayBuffer[(String, Type)],
    rettype: Type,
    body: ArrayBuffer[BB],
)

case class BB(instrs: ArrayBuffer[Instr]) extends Value
