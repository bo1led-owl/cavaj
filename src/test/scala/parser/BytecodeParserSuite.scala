package cavaj.parser

import cavaj.ir.*
import munit.FunSuite

import java.nio.file.{Files, Path}
import java.util.Comparator
import scala.sys.process.*
import scala.io.StdIn.readLine

class BytecodeParserSuite extends FunSuite {
  private def cleanup(path: Path): Unit = {
    Files
      .walk(path)
      .sorted(Comparator.reverseOrder())
      .forEach(Files.delete(_))
  }

  private def toClass(name: String, code: String): IrClass = {
    val temp = Files.createTempDirectory("cavaj-test-")
    val src  = temp.resolve(s"$name.java")
    Files.writeString(src, code)
    s"javac --release 21 ${src.toAbsolutePath.toString}".!

    val path    = temp.resolve(s"$name.class")
    val irClass = BytecodeParser.parseClassFile(path.toString)
    cleanup(temp)
    return irClass
  }

  private def printCmp(code: String, cls: IrClass): Unit = {
    val irLines   = cls.toString.linesIterator.toSeq
    val codeLines = code.linesIterator.toSeq.tail
    val width     = irLines.map(_.length).max
    val maxLines  = irLines.length max codeLines.length

    val sep = "-" * (width + 50)

     println(sep)
     println(s"${"parsed".padTo(width, ' ')}   | source")
     println(sep)

    for i <- 0 until maxLines do
      val left  = if i < irLines.length then irLines(i) else ""
      val right = if i < codeLines.length then codeLines(i) else ""
       println(s"${left.padTo(width, ' ')}   | $right")

     println(sep)
  }

  test("simple while loop") {
    val code =
      """
      |class While {
      |    int whileTest(int a, int b) {
      |        int sum = a * b;
      |        while (sum > a) {
      |            sum -= b;
      |        }
      |        return sum;
      |    }
      |}
      """.stripMargin

    val irClass = toClass("While", code)
    printCmp(code, irClass)
    assert(irClass.name == "While")

    val method = irClass.methods("whileTest").head
    val body = method.body.get

    assertEquals(body.bbs.size, 4, "expected 4 BBs")

    val v1 = Variable(cavaj.Type.Int, 1)
    val v2 = Variable(cavaj.Type.Int, 2)
    val v3 = Variable(cavaj.Type.Int, 3)
    val v4 = Variable(cavaj.Type.Int, 4)
    val v5 = Variable(cavaj.Type.Int, 5)
    val v6 = Variable(cavaj.Type.Int, 6)
    val v7 = Variable(cavaj.Type.Int, 7)
    val v8 = Variable(cavaj.Type.Int, 8)
    val v9 = Variable(cavaj.Type.Int, 9)

    val bb0 = body.bbs(0).toList
    val expectedBB0 = List(
      Push(v1),
      Push(v2),
      Pop(v3),
      Pop(v4),
      Push(Mul(v4, v3)),
      Pop(v3),
      Goto(1),
    )
    assertEquals(bb0, expectedBB0)

    val bb1 = body.bbs(1).toList
    val expectedBB1 = List(
      Push(v3),
      Push(v1),
      Pop(v5),
      Pop(v6),
      Br(CmpLe(v6, v5), 3, 2),
    )
    assertEquals(bb1, expectedBB1)

    val bb2 = body.bbs(2).toList
    val expectedBB2 = List(
      Push(v3),
      Push(v2),
      Pop(v7),
      Pop(v8),
      Push(Sub(v8, v7)),
      Pop(v3),
      Goto(1),
    )
    assertEquals(bb2, expectedBB2)

    val bb3 = body.bbs(3).toList
    val expectedBB3 = List(
      Push(v3),
      Pop(v9),
      Return(v9),
    )
    assertEquals(bb3, expectedBB3)
  }

  test("if with no else") {
    val code =
      """
      |class IfNoElse {
      |    int f(int a, int b) {
      |        if (a > b) {
      |            return a;
      |        }
      |        return b;
      |    }
      |}
    """.stripMargin
    val irClass = toClass("IfNoElse", code)
    printCmp(code, irClass)

    val method = irClass.methods("f").head
    val body = method.body.get

    assertEquals(body.bbs.size, 3, "expected 3 BBs")

    val v1 = Variable(cavaj.Type.Int, 1)
    val v2 = Variable(cavaj.Type.Int, 2)
    val v3 = Variable(cavaj.Type.Int, 3)
    val v4 = Variable(cavaj.Type.Int, 4)
    val v5 = Variable(cavaj.Type.Int, 5)
    val v6 = Variable(cavaj.Type.Int, 6)

    val bb0 = body.bbs(0).toList
    val expectedBB0 = List(
      Push(v1),
      Push(v2),
      Pop(v3),
      Pop(v4),
      Br(CmpLe(v4, v3), 2, 1)
    )
    assertEquals(bb0, expectedBB0)

    val bb1 = body.bbs(1).toList
    val expectedBB1 = List(
      Push(v1),
      Pop(v5),
      Return(v5)
    )
    assertEquals(bb1, expectedBB1)

    val bb2 = body.bbs(2).toList
    val expectedBB2 = List(
      Push(v2),
      Pop(v6),
      Return(v6)
    )
    assertEquals(bb2, expectedBB2)
  }

  test("interface test") {
    val code = """
      |interface Interface {
      |    int abstractMethod(int a, int b);
      |    public static void staticMethod() {
      |        System.out.println("hello");
      |    }
      |}
      """.stripMargin

    val irClass = toClass("Interface", code)
    printCmp(code, irClass)
    assert(irClass.name == "Interface")

    val abstractMethod = irClass.methods("abstractMethod").head
    assert(abstractMethod.body.isEmpty)
    assert(abstractMethod.parameters.size == 2)
    assert(abstractMethod.rettype == cavaj.Type.Int)

    val staticMethod = irClass.methods("staticMethod").head
    val body = staticMethod.body.get
    assertEquals(body.bbs.size, 2)
  }

  test("not") {
    val code =
      """
      |class Not {
      |    boolean state = false;
      |    void toggleState() {
      |        this.state = !this.state;
      |    }
      |}
      """.stripMargin

    val irClass = toClass("Not", code)
    printCmp(code, irClass)
    assert(irClass.name == "Not")

    val field = irClass.fields("state")
    assert(field.ty == cavaj.Type.Boolean)

    val method = irClass.methods("toggleState").head
    val body = method.body.get

    assertEquals(body.bbs.size, 5, "expected 5 BBs")

    val v0 = Variable(cavaj.Type.Reference("Not"), 0)
    val v1 = Variable(cavaj.Type.Reference(), 1)
    val v2 = Variable(cavaj.Type.Int, 2)
    val v3 = Variable(cavaj.Type.Undef, 3)
    val v4 = Variable(cavaj.Type.Reference(), 4)
    val v5 = Variable(cavaj.Type.Int, 5)
    val v6 = Variable(cavaj.Type.Reference(), 6)

    val bb0 = body.bbs(0).toList
    val expectedBB0 = List(
      Push(v0),
      Push(v0),
      Pop(v1),
      Push(GetField(v1, "state", cavaj.Type.Undef)),
      Pop(v2),
      Br(CmpNe(v2, IntLit(0)), 2, 1)
    )
    assertEquals(bb0, expectedBB0)

    val bb1 = body.bbs(1).toList
    val expectedBB1 = List(
      Push(IntLit(1)),
      Goto(3)
    )
    assertEquals(bb1, expectedBB1)

    val bb2 = body.bbs(2).toList
    val expectedBB2 = List(
      Push(IntLit(0)),
      Goto(3)
    )
    assertEquals(bb2, expectedBB2)

    val bb3 = body.bbs(3).toList
    val expectedBB3 = List(
      Pop(v3),
      Pop(v4),
      PutField(v4, "state", v3),
      Goto(4)
    )
    assertEquals(bb3, expectedBB3)

    val bb4 = body.bbs(4).toList
    val expectedBB4 = List(
      VoidReturn,
    )
    assertEquals(bb4, expectedBB4)
  }

  test("ternary") {
    val code =
      """
      |class Ternary {
      |    int ternary(boolean cond) {
      |        return cond ? 10 : 20;
      |    }
      |}
      """.stripMargin

    val irClass = toClass("Ternary", code)
    printCmp(code, irClass)
    assert(irClass.name == "Ternary")

    val method = irClass.methods("ternary").head
    val body = method.body.get

    assertEquals(body.bbs.size, 4, "expected 4 BBs")

    val v1 = Variable(cavaj.Type.Boolean, 1)
    val v2 = Variable(cavaj.Type.Int, 2)
    val v3 = Variable(cavaj.Type.Int, 3)
    val v4 = Variable(cavaj.Type.Int, 4)
    val v5 = Variable(cavaj.Type.Int, 5)

    val bb0 = body.bbs(0).toList
    val expectedBB0 = List(
      Push(v1),
      Pop(v2),
      Br(CmpEq(v2, IntLit(0)), 2, 1)
    )
    assertEquals(bb0, expectedBB0)

    val bb1 = body.bbs(1).toList
    val expectedBB1 = List(
      Push(ByteLit(10)),
      Goto(3)
    )
    assertEquals(bb1, expectedBB1)

    val bb2 = body.bbs(2).toList
    val expectedBB2 = List(
      Push(ByteLit(20)),
      Goto(3),
    )
    assertEquals(bb2, expectedBB2)

    val bb3 = body.bbs(3).toList
    val expectedBB3 = List(
      Pop(v3),
      Return(v3)
    )
    assertEquals(bb3, expectedBB3)
  }

}
