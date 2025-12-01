package cavaj
package parser

import cavaj.ir.*
import cavaj.{Type as CavajType, *}
import org.objectweb.asm.{
  ClassReader,
  ClassVisitor,
  FieldVisitor,
  Label,
  MethodVisitor,
  Opcodes,
  Type as AsmType,
}

import java.io.FileInputStream
import scala.collection.mutable
import scala.collection.mutable.{
  ArrayBuffer,
  LinkedHashMap,
  Map as MutableMap,
  Stack as MutableStack,
}

private def mapAccessToQualifiers(access: Int): Seq[Qualifier] = {
  val qualifiers = ArrayBuffer[Qualifier]()
  if (access & Opcodes.ACC_PUBLIC) != 0 then qualifiers += Qualifier.Public
  if (access & Opcodes.ACC_PRIVATE) != 0 then qualifiers += Qualifier.Private
  if (access & Opcodes.ACC_PROTECTED) != 0 then qualifiers += Qualifier.Protected
  if (access & Opcodes.ACC_STATIC) != 0 then qualifiers += Qualifier.Static
  if (access & Opcodes.ACC_FINAL) != 0 then qualifiers += Qualifier.Final
  if (access & Opcodes.ACC_ABSTRACT) != 0 then qualifiers += Qualifier.Abstract
  if qualifiers.isEmpty && (access & (Opcodes.ACC_PUBLIC | Opcodes.ACC_PRIVATE | Opcodes.ACC_PROTECTED)) == 0
  then {
    qualifiers += Qualifier.Default
  }
  qualifiers.toSeq
}

private def asmTypeToCavajType(asmType: AsmType): CavajType = asmType.getSort match {
  case AsmType.VOID    => CavajType.Void
  case AsmType.BOOLEAN => CavajType.Boolean
  case AsmType.CHAR    => CavajType.Char
  case AsmType.BYTE    => CavajType.Byte
  case AsmType.SHORT   => CavajType.Short
  case AsmType.INT     => CavajType.Int
  case AsmType.FLOAT   => CavajType.Float
  case AsmType.LONG    => CavajType.Long
  case AsmType.DOUBLE  => CavajType.Double
  case AsmType.ARRAY   => CavajType.Array(asmTypeToCavajType(asmType.getElementType))
  case AsmType.OBJECT  => CavajType.Reference(asmType.getClassName.replace('/', '.'))
  case _               => CavajType.Undef
}

private case class UninitializedRef(c: String) extends Value {
  override def ty: Type         = CavajType.Reference(c)
  override def toString: String = s"uninitialized($c)"
}

object BytecodeParser {
  def parseClassFile(path: String): IrClass = {
    val s = new FileInputStream(path)
    try {
      val reader  = new ClassReader(s)
      val visitor = new ClassBuilder()
      reader.accept(visitor, ClassReader.EXPAND_FRAMES)
      visitor.getIrClass
    } finally {
      s.close()
    }
  }
}

private class ClassBuilder extends ClassVisitor(Opcodes.ASM9) {
  private var className: String          = null
  private var qualifiers: Seq[Qualifier] = Seq.empty
  private var superName: Option[String]  = None
  private val interfaces                 = ArrayBuffer[String]()
  private val fields                     = LinkedHashMap[String, Field]()
  private val methods                    = LinkedHashMap[String, ArrayBuffer[IrMethod]]()

  def getIrClass: IrClass = {
    Class(
      qualifiers,
      className,
      fields.toMap,
      methods.view.mapValues(_.toSeq).toMap,
      interfaces.toSeq,
      superName,
    )
  }

  override def visit(
      version: Int,
      access: Int,
      name: String,
      signature: String,
      superName: String,
      interfaces: Array[String],
  ): Unit = {
    this.className = name.replace('/', '.')
    this.qualifiers = mapAccessToQualifiers(access)
    this.superName = Option(superName).map(_.replace('/', '.'))
    if interfaces != null then {
      this.interfaces ++= interfaces.map(_.replace('/', '.'))
    }
  }

  override def visitField(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      value: Any,
  ): FieldVisitor = {
    val ty = asmTypeToCavajType(AsmType.getType(descriptor))
    val constval = Option(value).map {
      case v: Integer          => IntLit(v)
      case v: java.lang.Long   => LongLit(v)
      case v: java.lang.Float  => FloatLit(v)
      case v: java.lang.Double => DoubleLit(v)
      case v: String           => StringLit(v)
      case v => throw new RuntimeException(s"Unsupported constant field value: $v")
    }

    fields(name) = Field(mapAccessToQualifiers(access), name, ty, constval)
    null
  }

  override def visitMethod(
      access: Int,
      name: String,
      descriptor: String,
      signature: String,
      exceptions: Array[String],
  ): MethodVisitor = {
    val isAbstract = (access & Opcodes.ACC_ABSTRACT) != 0
    new MethodBuilder(
      access,
      name,
      descriptor,
      this.className,
      isAbstract,
      irMethod => {
        methods.getOrElseUpdate(name, ArrayBuffer()).addOne(irMethod)
      },
    )
  }
}

private class MethodBuilder(
    access: Int,
    name: String,
    descriptor: String,
    className: String,
    isAbstract: Boolean,
    onComplete: IrMethod => Unit,
) extends MethodVisitor(Opcodes.ASM9) {

  // temporary representation for instructions before labels are resolved to indices.
  private sealed trait WipInstr
  private case class FinalInstr(i: Instr)                              extends WipInstr
  private case class WipGoto(target: Label)                            extends WipInstr
  private case class WipBr(cond: Value, onTrue: Label, onFalse: Label) extends WipInstr

  private val operandStack = MutableStack[Value]()
  private val localVars    = MutableMap[Int, Variable]()

  // Wip IR
  private val instructions      = ArrayBuffer[WipInstr]()
  private val labelToInstrIndex = MutableMap[Label, Int]()
  private val jumpTargets       = mutable.Set[Int]()

  private val methodType = AsmType.getMethodType(descriptor)
  private val qualifiers = mapAccessToQualifiers(access)
  private val paramTypes = methodType.getArgumentTypes.map(asmTypeToCavajType)
  private val returnType = asmTypeToCavajType(methodType.getReturnType)
  private val params     = LinkedHashMap[String, CavajType]()

  var currentLocalVarIndex = 0
  if !qualifiers.contains(Qualifier.Static) then {
    localVars(0) = Variable(CavajType.Reference(className), 0)
    currentLocalVarIndex += 1
  }
  paramTypes.zipWithIndex.foreach { case (ty, i) =>
    val name = s"a${i + 1}"
    params(name) = ty
    localVars(currentLocalVarIndex) = Variable(ty, currentLocalVarIndex)
    currentLocalVarIndex += (if ty == CavajType.Long || ty == CavajType.Double then 2 else 1)
  }

  private def getLocalVar(index: Int, ty: CavajType): Variable = {
    localVars.getOrElseUpdate(index, Variable(ty, index))
  }

  private def addInstr(instr: Instr): Unit = {
    instructions += FinalInstr(instr)
  }

  override def visitInsn(opcode: Int): Unit = opcode match {
    case Opcodes.ACONST_NULL => operandStack.push(NullLit())
    case Opcodes.ICONST_M1   => operandStack.push(IntLit(-1))
    case Opcodes.ICONST_0    => operandStack.push(IntLit(0))
    case Opcodes.ICONST_1    => operandStack.push(IntLit(1))
    case Opcodes.ICONST_2    => operandStack.push(IntLit(2))
    case Opcodes.ICONST_3    => operandStack.push(IntLit(3))
    case Opcodes.ICONST_4    => operandStack.push(IntLit(4))
    case Opcodes.ICONST_5    => operandStack.push(IntLit(5))
    case Opcodes.LCONST_0    => operandStack.push(LongLit(0))
    case Opcodes.LCONST_1    => operandStack.push(LongLit(1))
    case Opcodes.FCONST_0    => operandStack.push(FloatLit(0))
    case Opcodes.FCONST_1    => operandStack.push(FloatLit(1))
    case Opcodes.FCONST_2    => operandStack.push(FloatLit(2))
    case Opcodes.DCONST_0    => operandStack.push(DoubleLit(0))
    case Opcodes.DCONST_1    => operandStack.push(DoubleLit(1))

    case Opcodes.IADD | Opcodes.LADD | Opcodes.FADD | Opcodes.DADD => pushBinary(Add.apply)
    case Opcodes.ISUB | Opcodes.LSUB | Opcodes.FSUB | Opcodes.DSUB => pushBinary(Sub.apply)
    case Opcodes.IMUL | Opcodes.LMUL | Opcodes.FMUL | Opcodes.DMUL => pushBinary(Mul.apply)
    case Opcodes.IDIV | Opcodes.LDIV | Opcodes.FDIV | Opcodes.DDIV => pushBinary(Div.apply)
    case Opcodes.IREM | Opcodes.LREM | Opcodes.FREM | Opcodes.DREM => pushBinary(Rem.apply)
    case Opcodes.INEG | Opcodes.LNEG | Opcodes.FNEG | Opcodes.DNEG =>
      operandStack.push(Negate(operandStack.pop()))
    case Opcodes.ISHL | Opcodes.LSHL   => pushBinary(Shl.apply)
    case Opcodes.ISHR | Opcodes.LSHR   => pushBinary(Shr.apply)
    case Opcodes.IUSHR | Opcodes.LUSHR => pushBinary(UShr.apply)
    case Opcodes.IAND | Opcodes.LAND   => pushBinary(BitAnd.apply)
    case Opcodes.IOR | Opcodes.LOR     => pushBinary(BitOr.apply)
    case Opcodes.IXOR | Opcodes.LXOR   => pushBinary(Xor.apply)

    case Opcodes.DUP => operandStack.push(operandStack.top)

    case Opcodes.I2L => operandStack.push(ToLong(operandStack.pop()))
    case Opcodes.I2F => operandStack.push(ToFloat(operandStack.pop()))
    case Opcodes.I2D => operandStack.push(ToDouble(operandStack.pop()))
    case Opcodes.L2I => operandStack.push(ToInt(operandStack.pop()))

    case Opcodes.ARRAYLENGTH => operandStack.push(ArrayLength(operandStack.pop()))
    case Opcodes.IALOAD | Opcodes.LALOAD | Opcodes.FALOAD | Opcodes.DALOAD | Opcodes.AALOAD |
        Opcodes.BALOAD | Opcodes.CALOAD | Opcodes.SALOAD =>
      val index = operandStack.pop()
      val arr   = operandStack.pop()
      operandStack.push(ArrayLoad(arr, index))
    case Opcodes.IASTORE | Opcodes.LASTORE | Opcodes.FASTORE | Opcodes.DASTORE | Opcodes.AASTORE |
        Opcodes.BASTORE | Opcodes.CASTORE | Opcodes.SASTORE =>
      val value = operandStack.pop()
      val index = operandStack.pop()
      val arr   = operandStack.pop()
      addInstr(ArrayStore(arr, index, value))

    case Opcodes.RETURN => addInstr(VoidReturn)
    case Opcodes.IRETURN | Opcodes.LRETURN | Opcodes.FRETURN | Opcodes.DRETURN | Opcodes.ARETURN =>
      addInstr(Return(operandStack.pop()))

    case op => throw Exception(s"Unknown opcode $op")
  }

  private def pushBinary(cons: (Value, Value) => BinaryInstr): Unit = {
    val rhs = operandStack.pop()
    val lhs = operandStack.pop()
    operandStack.push(cons(lhs, rhs))
  }

  override def visitVarInsn(opcode: Int, varIndex: Int): Unit = opcode match {
    case Opcodes.ILOAD => operandStack.push(getLocalVar(varIndex, CavajType.Int))
    case Opcodes.LLOAD => operandStack.push(getLocalVar(varIndex, CavajType.Long))
    case Opcodes.FLOAD => operandStack.push(getLocalVar(varIndex, CavajType.Float))
    case Opcodes.DLOAD => operandStack.push(getLocalVar(varIndex, CavajType.Double))
    case Opcodes.ALOAD => operandStack.push(getLocalVar(varIndex, CavajType.Reference()))
    case Opcodes.ISTORE | Opcodes.LSTORE | Opcodes.FSTORE | Opcodes.DSTORE | Opcodes.ASTORE =>
      val value    = operandStack.pop()
      val variable = getLocalVar(varIndex, value.ty)
      addInstr(Load(variable, value))
  }

  override def visitIntInsn(opcode: Int, operand: Int): Unit = opcode match {
    case Opcodes.BIPUSH => operandStack.push(ByteLit(operand.toByte))
    case Opcodes.SIPUSH => operandStack.push(ShortLit(operand.toShort))
    case Opcodes.NEWARRAY =>
      val elemType = operand match {
        case Opcodes.T_BOOLEAN => CavajType.Boolean
        case Opcodes.T_CHAR    => CavajType.Char
        case Opcodes.T_FLOAT   => CavajType.Float
        case Opcodes.T_DOUBLE  => CavajType.Double
        case Opcodes.T_BYTE    => CavajType.Byte
        case Opcodes.T_SHORT   => CavajType.Short
        case Opcodes.T_INT     => CavajType.Int
        case Opcodes.T_LONG    => CavajType.Long
        case _ => throw new IllegalArgumentException(s"Unknown NEWARRAY type: $operand")
      }
      operandStack.push(NewArray(elemType, operandStack.pop()))
  }

  override def visitLdcInsn(value: Any): Unit = value match {
    case v: Integer          => operandStack.push(IntLit(v))
    case v: java.lang.Long   => operandStack.push(LongLit(v))
    case v: java.lang.Float  => operandStack.push(FloatLit(v))
    case v: java.lang.Double => operandStack.push(DoubleLit(v))
    case v: String           => operandStack.push(StringLit(v))
    case v: AsmType if v.getSort == AsmType.OBJECT =>
      operandStack.push(StringLit(v.getClassName.replace('/', '.')))
    case _ => throw new RuntimeException(s"Unsupported LDC value: $value")
  }

  override def visitTypeInsn(opcode: Int, typeName: String): Unit = opcode match {
    case Opcodes.NEW => operandStack.push(UninitializedRef(typeName.replace('/', '.')))
    case Opcodes.ANEWARRAY =>
      val len      = operandStack.pop()
      val elemType = CavajType.Reference(typeName.replace('/', '.'))
      operandStack.push(NewArray(elemType, len))
    case Opcodes.INSTANCEOF =>
      val obj = operandStack.pop()
      operandStack.push(InstanceOf(obj, typeName.replace('/', '.')))
  }

  override def visitFieldInsn(
      opcode: Int,
      owner: String,
      name: String,
      descriptor: String,
  ): Unit = {
    val ownerName = owner.replace('/', '.')
    opcode match {
      case Opcodes.GETSTATIC => operandStack.push(GetStaticField(ownerName, name))
      case Opcodes.PUTSTATIC => addInstr(PutStaticField(ownerName, name, operandStack.pop()))
      case Opcodes.GETFIELD  => operandStack.push(GetField(operandStack.pop(), name))
      case Opcodes.PUTFIELD =>
        val value = operandStack.pop(); val obj = operandStack.pop();
        addInstr(PutField(obj, name, value))
    }
  }

  override def visitMethodInsn(
      opcode: Int,
      owner: String,
      name: String,
      descriptor: String,
      isInterface: Boolean,
  ): Unit = {
    val methodType = AsmType.getMethodType(descriptor)
    val retType    = asmTypeToCavajType(methodType.getReturnType)

    val args = new ArrayBuffer[Value]()
    for _ <- methodType.getArgumentTypes.indices do {
      args.prepend(operandStack.pop())
    }

    if opcode == Opcodes.INVOKESPECIAL && name == "<init>" then {
      val objRef = operandStack.pop()

      objRef match {
        // initializing a new object
        case uninit: UninitializedRef =>
          val newInstr = New(uninit.c, args.toSeq)
          val idx      = operandStack.indexOf(uninit)
          if idx != -1 then {
            operandStack(idx) = newInstr
          } else {
            addInstr(newInstr)
          }
          return

        // superclass (sibling??) constructor
        case v: Variable if v.index == 0 =>
          val constructorCall =
            InvokeStaticMethod(owner.replace('/', '.'), name, args.toSeq, retType)
          addInstr(constructorCall)
          return

        // 💀
        case _ =>
          operandStack.push(objRef)
      }
    }

    val result: Instr = opcode match {
      case Opcodes.INVOKESTATIC =>
        InvokeStaticMethod(owner.replace('/', '.'), name, args.toSeq, retType)

      case _ => // INVOKEVIRTUAL, INVOKEINTERFACE, etc
        val obj = operandStack.pop()
        InvokeInstanceMethod(obj, name, args.toSeq, retType)
    }

    if retType != CavajType.Void then {
      operandStack.push(result)
    } else {
      addInstr(result)
    }
  }

  override def visitJumpInsn(opcode: Int, label: Label): Unit = {
    if opcode == Opcodes.GOTO then {
      instructions += WipGoto(label)
      return
    }

    val onTrueLabel  = label
    val onFalseLabel = new Label()

    val cond = opcode match {
      case Opcodes.IFEQ      => CmpEq(operandStack.pop(), IntLit(0))
      case Opcodes.IFNE      => CmpNe(operandStack.pop(), IntLit(0))
      case Opcodes.IFLT      => CmpLt(operandStack.pop(), IntLit(0))
      case Opcodes.IFGE      => CmpGe(operandStack.pop(), IntLit(0))
      case Opcodes.IFGT      => CmpGt(operandStack.pop(), IntLit(0))
      case Opcodes.IFLE      => CmpLe(operandStack.pop(), IntLit(0))
      case Opcodes.IFNULL    => CmpEq(operandStack.pop(), NullLit())
      case Opcodes.IFNONNULL => CmpNe(operandStack.pop(), NullLit())
      case Opcodes.IF_ICMPEQ | Opcodes.IF_ACMPEQ =>
        val r = operandStack.pop(); val l = operandStack.pop(); CmpEq(l, r)
      case Opcodes.IF_ICMPNE | Opcodes.IF_ACMPNE =>
        val r = operandStack.pop(); val l = operandStack.pop(); CmpNe(l, r)
      case Opcodes.IF_ICMPLT => val r = operandStack.pop(); val l = operandStack.pop(); CmpLt(l, r)
      case Opcodes.IF_ICMPGE => val r = operandStack.pop(); val l = operandStack.pop(); CmpGe(l, r)
      case Opcodes.IF_ICMPGT => val r = operandStack.pop(); val l = operandStack.pop(); CmpGt(l, r)
      case Opcodes.IF_ICMPLE => val r = operandStack.pop(); val l = operandStack.pop(); CmpLe(l, r)
    }

    instructions += WipBr(cond, onTrueLabel, onFalseLabel)
    visitLabel(onFalseLabel)
  }

  override def visitLabel(label: Label): Unit = {
    labelToInstrIndex(label) = instructions.length
    jumpTargets += instructions.length
  }

  override def visitEnd(): Unit = {
    if isAbstract then {
      onComplete(Method(qualifiers, name, params, returnType, None))
      return
    }
    val bbStarts = jumpTargets.toSeq.sorted.distinct
    if bbStarts.isEmpty then {
      val body = if instructions.nonEmpty then {
        val finalInstrs = instructions.collect { case FinalInstr(i) => i }
        IndexedSeq(BB(finalInstrs.toIndexedSeq))
      } else IndexedSeq.empty
      onComplete(Method(qualifiers, name, params, returnType, Some(body)))
      return
    }

    val bbRanges = bbStarts.zip(bbStarts.tail :+ instructions.length)
    val bbs      = ArrayBuffer[BB]()

    val instrIndexToBbIndex = bbStarts.zipWithIndex.toMap
    val labelToBbIndex = labelToInstrIndex.map { case (lbl, instrIdx) =>
      val bbStartIdx = bbStarts.reverse.find(_ <= instrIdx).getOrElse(0)
      lbl -> instrIndexToBbIndex(bbStartIdx)
    }.toMap

    for ((start, end), bbIndex) <- bbRanges.zipWithIndex do {
      val bbWipInstrs = instructions.slice(start, end)

      val bbFinalInstrs = bbWipInstrs.flatMap {
        case WipGoto(targetLabel) => Some(Goto(labelToBbIndex(targetLabel)))
        case WipBr(cond, onTrueLabel, onFalseLabel) =>
          Some(Br(cond, labelToBbIndex(onTrueLabel), labelToBbIndex(onFalseLabel)))
        case FinalInstr(i) => Some(i)
      }.toBuffer

      val lastIsTerminator = bbWipInstrs.lastOption match {
        case Some(WipGoto(_)) | Some(WipBr(_, _, _)) => true
        case Some(FinalInstr(i))                     => i.isInstanceOf[TerminatorInstr]
        case _                                       => false
      }

      if !lastIsTerminator && bbIndex + 1 < bbStarts.length then {
        bbFinalInstrs += Goto(bbIndex + 1)
      }

      bbs += BB(bbFinalInstrs.toIndexedSeq)
    }

    onComplete(Method(qualifiers, name, params, returnType, Some(bbs.toIndexedSeq)))
  }
}
