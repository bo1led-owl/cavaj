package cavaj
package ir

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap

case class Class(
    name: String,
    staticFields: HashMap[String, (Type, Value)],
    fields: HashMap[String, Type],
    staticMethods: HashMap[String, Method],
    methods: HashMap[String, Method],
)

case class Method(
    parameters: LinkedHashMap[String, (Type, Value)],
    rettype: Type,
    body: ArrayBuffer[BB],
)

case class BB(instrs: ArrayBuffer[Instr]) extends Value
