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

  private val localVars = MutableMap[Int, Variable]()

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
  paramTypes.zipWithIndex.foreach { (ty, i) =>
    val name = s"v${i + currentLocalVarIndex}"
    params(name) = ty
    localVars(currentLocalVarIndex) = Variable(ty, currentLocalVarIndex)
    currentLocalVarIndex += (if ty == CavajType.Long || ty == CavajType.Double then 2 else 1)
  }

  private var currentTempVarIndex = currentLocalVarIndex

  private def getLocalVar(index: Int, ty: CavajType): Variable = {
    localVars.getOrElseUpdate(index, Variable(ty, index))
  }

  private def freshTemp(ty: CavajType): Variable = {
    val v = Variable(ty, currentTempVarIndex)
    currentTempVarIndex += (if ty == CavajType.Long || ty == CavajType.Double then 2 else 1)
    v
  }

  private def addInstr(instr: Instr): Unit = {
    instructions += FinalInstr(instr)
  }

  override def visitInsn(opcode: Int): Unit = opcode match {
    case Opcodes.ACONST_NULL => addInstr(Push(NullLit()))
    case Opcodes.ICONST_M1   => addInstr(Push(IntLit(-1)))
    case Opcodes.ICONST_0    => addInstr(Push(IntLit(0)))
    case Opcodes.ICONST_1    => addInstr(Push(IntLit(1)))
    case Opcodes.ICONST_2    => addInstr(Push(IntLit(2)))
    case Opcodes.ICONST_3    => addInstr(Push(IntLit(3)))
    case Opcodes.ICONST_4    => addInstr(Push(IntLit(4)))
    case Opcodes.ICONST_5    => addInstr(Push(IntLit(5)))
    case Opcodes.LCONST_0    => addInstr(Push(LongLit(0)))
    case Opcodes.LCONST_1    => addInstr(Push(LongLit(1)))
    case Opcodes.FCONST_0    => addInstr(Push(FloatLit(0)))
    case Opcodes.FCONST_1    => addInstr(Push(FloatLit(1)))
    case Opcodes.FCONST_2    => addInstr(Push(FloatLit(2)))
    case Opcodes.DCONST_0    => addInstr(Push(DoubleLit(0)))
    case Opcodes.DCONST_1    => addInstr(Push(DoubleLit(1)))

    case Opcodes.IADD => pushBinary(CavajType.Int, Add.apply)
    case Opcodes.LADD => pushBinary(CavajType.Long, Add.apply)
    case Opcodes.FADD => pushBinary(CavajType.Float, Add.apply)
    case Opcodes.DADD => pushBinary(CavajType.Double, Add.apply)

    case Opcodes.ISUB => pushBinary(CavajType.Int, Sub.apply)
    case Opcodes.LSUB => pushBinary(CavajType.Long, Sub.apply)
    case Opcodes.FSUB => pushBinary(CavajType.Float, Sub.apply)
    case Opcodes.DSUB => pushBinary(CavajType.Double, Sub.apply)

    case Opcodes.IMUL => pushBinary(CavajType.Int, Mul.apply)
    case Opcodes.LMUL => pushBinary(CavajType.Long, Mul.apply)
    case Opcodes.FMUL => pushBinary(CavajType.Float, Mul.apply)
    case Opcodes.DMUL => pushBinary(CavajType.Double, Mul.apply)

    case Opcodes.IDIV => pushBinary(CavajType.Int, Div.apply)
    case Opcodes.LDIV => pushBinary(CavajType.Long, Div.apply)
    case Opcodes.FDIV => pushBinary(CavajType.Float, Div.apply)
    case Opcodes.DDIV => pushBinary(CavajType.Double, Div.apply)

    case Opcodes.IREM => pushBinary(CavajType.Int, Rem.apply)
    case Opcodes.LREM => pushBinary(CavajType.Long, Rem.apply)
    case Opcodes.FREM => pushBinary(CavajType.Float, Rem.apply)
    case Opcodes.DREM => pushBinary(CavajType.Double, Rem.apply)

    case Opcodes.INEG | Opcodes.LNEG | Opcodes.FNEG | Opcodes.DNEG =>
      val ty =
        if opcode == Opcodes.INEG then CavajType.Int
        else if opcode == Opcodes.LNEG then CavajType.Long
        else if opcode == Opcodes.FNEG then CavajType.Float
        else CavajType.Double
      val t = freshTemp(ty)
      addInstr(Pop(t))
      addInstr(Push(Negate(t)))

    // POV: you are working with the most sane bytecode
    case Opcodes.ISHL => pushBinary(CavajType.Int, Shl.apply)
    case Opcodes.LSHL =>
      pushBinary(CavajType.Long, Shl.apply)
      val rhs = freshTemp(CavajType.Int)
      val lhs = freshTemp(CavajType.Long)
      addInstr(Pop(rhs))
      addInstr(Pop(lhs))
      addInstr(Push(Shl(lhs, rhs)))

    case Opcodes.ISHR => pushBinary(CavajType.Int, Shr.apply)
    case Opcodes.LSHR =>
      val rhs = freshTemp(CavajType.Int)
      val lhs = freshTemp(CavajType.Long)
      addInstr(Pop(rhs))
      addInstr(Pop(lhs))
      addInstr(Push(Shr(lhs, rhs)))

    case Opcodes.IUSHR => pushBinary(CavajType.Int, UShr.apply)
    case Opcodes.LUSHR =>
      val rhs = freshTemp(CavajType.Int)
      val lhs = freshTemp(CavajType.Long)
      addInstr(Pop(rhs))
      addInstr(Pop(lhs))
      addInstr(Push(UShr(lhs, rhs)))

    case Opcodes.IAND => pushBinary(CavajType.Int, BitAnd.apply)
    case Opcodes.LAND => pushBinary(CavajType.Long, BitAnd.apply)
    case Opcodes.IOR  => pushBinary(CavajType.Int, BitOr.apply)
    case Opcodes.LOR  => pushBinary(CavajType.Long, BitOr.apply)
    case Opcodes.IXOR => pushBinary(CavajType.Int, Xor.apply)
    case Opcodes.LXOR => pushBinary(CavajType.Long, Xor.apply)

    case Opcodes.DUP =>
      val t = freshTemp(CavajType.Undef)
      addInstr(Pop(t))
      addInstr(Push(t))
      addInstr(Push(t))

    case Opcodes.POP =>
      val t = freshTemp(CavajType.Undef)
      addInstr(Pop(t))

    case Opcodes.I2L => pushCast(CavajType.Int, CavajType.Long)
    case Opcodes.I2F => pushCast(CavajType.Int, CavajType.Float)
    case Opcodes.I2D => pushCast(CavajType.Int, CavajType.Double)
    case Opcodes.L2I => pushCast(CavajType.Long, CavajType.Int)

    case Opcodes.ARRAYLENGTH =>
      val t = freshTemp(CavajType.Undef)
      addInstr(Pop(t))
      addInstr(Push(ArrayLength(t)))

    case Opcodes.IALOAD | Opcodes.LALOAD | Opcodes.FALOAD | Opcodes.DALOAD | Opcodes.AALOAD |
        Opcodes.BALOAD | Opcodes.CALOAD | Opcodes.SALOAD =>
      val index = freshTemp(CavajType.Int)
      val arr   = freshTemp(CavajType.Undef)
      addInstr(Pop(index))
      addInstr(Pop(arr))
      addInstr(Push(ArrayLoad(arr, index)))

    case Opcodes.IASTORE | Opcodes.LASTORE | Opcodes.FASTORE | Opcodes.DASTORE | Opcodes.AASTORE |
        Opcodes.BASTORE | Opcodes.CASTORE | Opcodes.SASTORE =>
      val value = freshTemp(CavajType.Undef)
      val index = freshTemp(CavajType.Int)
      val arr   = freshTemp(CavajType.Undef)
      addInstr(Pop(value))
      addInstr(Pop(index))
      addInstr(Pop(arr))
      addInstr(ArrayStore(arr, index, value))

    case Opcodes.RETURN => addInstr(VoidReturn)
    case Opcodes.IRETURN | Opcodes.LRETURN | Opcodes.FRETURN | Opcodes.DRETURN | Opcodes.ARETURN =>
      val t = freshTemp(returnType)
      addInstr(Pop(t))
      addInstr(Return(t))

    case op => throw Exception(s"Unknown opcode $op")
  }

  private def pushBinary(ty: CavajType, cons: (Value, Value) => BinaryInstr): Unit = {
    val rhs = freshTemp(ty)
    val lhs = freshTemp(ty)
    addInstr(Pop(rhs))
    addInstr(Pop(lhs))
    addInstr(Push(cons(lhs, rhs)))
  }

  private def pushCast(from: CavajType, to: CavajType): Unit = {
    val t = freshTemp(from)
    addInstr(Pop(t))
    addInstr(Push(CastInstr(to, t)))
  }

  override def visitVarInsn(opcode: Int, varIndex: Int): Unit = opcode match {
    case Opcodes.ILOAD => addInstr(Push(getLocalVar(varIndex, CavajType.Int)))
    case Opcodes.LLOAD => addInstr(Push(getLocalVar(varIndex, CavajType.Long)))
    case Opcodes.FLOAD => addInstr(Push(getLocalVar(varIndex, CavajType.Float)))
    case Opcodes.DLOAD => addInstr(Push(getLocalVar(varIndex, CavajType.Double)))
    case Opcodes.ALOAD => addInstr(Push(getLocalVar(varIndex, CavajType.Reference())))
    case Opcodes.ISTORE | Opcodes.LSTORE | Opcodes.FSTORE | Opcodes.DSTORE | Opcodes.ASTORE =>
      val ty = opcode match {
        case Opcodes.ISTORE => CavajType.Int
        case Opcodes.LSTORE => CavajType.Long
        case Opcodes.FSTORE => CavajType.Float
        case Opcodes.DSTORE => CavajType.Double
        case Opcodes.ASTORE => CavajType.Reference()
        case _              => CavajType.Undef
      }
      addInstr(Pop(getLocalVar(varIndex, ty)))
  }

  override def visitIntInsn(opcode: Int, operand: Int): Unit = opcode match {
    case Opcodes.BIPUSH => addInstr(Push(ByteLit(operand.toByte)))
    case Opcodes.SIPUSH => addInstr(Push(ShortLit(operand.toShort)))
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
      val len = freshTemp(CavajType.Int)
      addInstr(Pop(len))
      addInstr(Push(NewArray(elemType, len)))
  }

  override def visitLdcInsn(value: Any): Unit = value match {
    case v: Integer          => addInstr(Push(IntLit(v)))
    case v: java.lang.Long   => addInstr(Push(LongLit(v)))
    case v: java.lang.Float  => addInstr(Push(FloatLit(v)))
    case v: java.lang.Double => addInstr(Push(DoubleLit(v)))
    case v: String           => addInstr(Push(StringLit(v)))
    case v: AsmType if v.getSort == AsmType.OBJECT =>
      addInstr(Push(StringLit(v.getClassName.replace('/', '.'))))
    case _ => throw new RuntimeException(s"Unsupported LDC value: $value")
  }

  override def visitTypeInsn(opcode: Int, typeName: String): Unit = opcode match {
    case Opcodes.NEW =>
      val c = typeName.replace('/', '.')
      addInstr(Push(New(c, Nil)))

    case Opcodes.ANEWARRAY =>
      val len      = freshTemp(CavajType.Int)
      val elemType = CavajType.Reference(typeName.replace('/', '.'))
      addInstr(Pop(len))
      addInstr(Push(NewArray(elemType, len)))

    case Opcodes.INSTANCEOF =>
      val obj = freshTemp(CavajType.Reference())
      addInstr(Pop(obj))
      addInstr(Push(InstanceOf(obj, typeName.replace('/', '.'))))
  }

  override def visitFieldInsn(
      opcode: Int,
      owner: String,
      name: String,
      descriptor: String,
  ): Unit = {
    val ownerName = owner.replace('/', '.')
    opcode match {
      case Opcodes.GETSTATIC => addInstr(Push(GetStaticField(ownerName, name)))
      case Opcodes.PUTSTATIC =>
        val v = freshTemp(CavajType.Undef)
        addInstr(Pop(v))
        addInstr(PutStaticField(ownerName, name, v))
      case Opcodes.GETFIELD =>
        val obj = freshTemp(CavajType.Reference())
        addInstr(Pop(obj))
        addInstr(Push(GetField(obj, name)))
      case Opcodes.PUTFIELD =>
        val value = freshTemp(CavajType.Undef)
        val obj   = freshTemp(CavajType.Reference())
        addInstr(Pop(value))
        addInstr(Pop(obj))
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

    val argsBuffer = new ArrayBuffer[Value]()
    for _ <- methodType.getArgumentTypes.indices do {
      val t = freshTemp(CavajType.Undef)
      addInstr(Pop(t))
      argsBuffer.prepend(t)
    }
    val args = argsBuffer.toSeq

    if opcode == Opcodes.INVOKESPECIAL && name == "<init>" then {
      val ref = freshTemp(CavajType.Reference())
      addInstr(Pop(ref))

      val call = InvokeInstanceMethod(ref, name, args, retType)
      addInstr(call)
      return
    }

    val result: Instr = opcode match {
      case Opcodes.INVOKESTATIC =>
        InvokeStaticMethod(owner.replace('/', '.'), name, args, retType)

      case _ => // INVOKEVIRTUAL, INVOKEINTERFACE, etc
        val obj = freshTemp(CavajType.Reference())
        addInstr(Pop(obj))
        InvokeInstanceMethod(obj, name, args, retType)
    }

    if retType != CavajType.Void then {
      addInstr(Push(result))
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

    def getCond(cmp: (Value, Value) => Value, val0: Boolean = false): Value = {
      val t = freshTemp(CavajType.Int)
      addInstr(Pop(t))
      if val0 then cmp(t, IntLit(0)) else cmp(t, NullLit())
    }

    def getCond2(cmp: (Value, Value) => Value): Value = {
      val rhs = freshTemp(CavajType.Int)
      val lhs = freshTemp(CavajType.Int)
      addInstr(Pop(rhs))
      addInstr(Pop(lhs))
      cmp(lhs, rhs)
    }

    val cond = opcode match {
      case Opcodes.IFEQ                          => getCond(CmpEq.apply, true)
      case Opcodes.IFNE                          => getCond(CmpNe.apply, true)
      case Opcodes.IFLT                          => getCond(CmpLt.apply, true)
      case Opcodes.IFGE                          => getCond(CmpGe.apply, true)
      case Opcodes.IFGT                          => getCond(CmpGt.apply, true)
      case Opcodes.IFLE                          => getCond(CmpLe.apply, true)
      case Opcodes.IFNULL                        => getCond(CmpEq.apply, false)
      case Opcodes.IFNONNULL                     => getCond(CmpNe.apply, false)
      case Opcodes.IF_ICMPEQ | Opcodes.IF_ACMPEQ => getCond2(CmpEq.apply)
      case Opcodes.IF_ICMPNE | Opcodes.IF_ACMPNE => getCond2(CmpNe.apply)
      case Opcodes.IF_ICMPLT                     => getCond2(CmpLt.apply)
      case Opcodes.IF_ICMPGE                     => getCond2(CmpGe.apply)
      case Opcodes.IF_ICMPGT                     => getCond2(CmpGt.apply)
      case Opcodes.IF_ICMPLE                     => getCond2(CmpLe.apply)
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
      onComplete(
        Method(
          qualifiers,
          name,
          params,
          returnType,
          Some(IrMethodBody(0, ArrayBuffer.from(body))),
        )
      )
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

    onComplete(
      Method(
        qualifiers,
        name,
        params,
        returnType,
        Some(IrMethodBody(0, ArrayBuffer.from(bbs))),
      )
    )
  }
}
