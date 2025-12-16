package cavaj
package analysis
package passes

import cavaj.ir.*
import cavaj.parser.BytecodeParser
import munit.FunSuite

import java.nio.file.{Files, Path}
import java.util.Comparator
import scala.sys.process.*

class StackEliminationSuite extends FunSuite {
  private def cleanup(path: Path): Unit = {
    Files
      .walk(path)
      .sorted(Comparator.reverseOrder())
      .forEach(Files.delete(_))
  }

  private def toClass(name: String, code: String): IrClass = {
    val temp = Files.createTempDirectory("cavaj-stack-test-")
    val src  = temp.resolve(s"$name.java")
    Files.writeString(src, code)
    s"javac --release 21 ${src.toAbsolutePath.toString}".!

    val path    = temp.resolve(s"$name.class")
    val irClass = BytecodeParser.parseClassFile(path.toString)
    cleanup(temp)

    StackElimination.run(irClass)
  }

  private def printCmp(code: String, cls: IrClass): Unit = {
    val irLines   = cls.toString.linesIterator.toSeq
    val codeLines = code.linesIterator.toSeq.tail
    val width     = if irLines.nonEmpty then irLines.map(_.length).max else 0
    val maxLines  = irLines.length max codeLines.length

    val sep = "-" * (width + 50)

    println(sep)
    println(s"${"transformed".padTo(width, ' ')}   | source")
    println(sep)

    for i <- 0 until maxLines do
      val left  = if i < irLines.length then irLines(i) else ""
      val right = if i < codeLines.length then codeLines(i) else ""
      println(s"${left.padTo(width, ' ')}   | $right")

    println(sep)
  }

  private def assertNoStackOps(cls: IrClass): Unit = {
    for {
      methods <- cls.methods.values
      method  <- methods
      body    <- method.body
      bb      <- body.bbs
      instr   <- bb
    } do {
      instr match {
        case _: Push | _: Pop => fail(s"Found stack op in ${method.name}: $instr")
        case _                => // OK
      }
    }
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

    assertNoStackOps(irClass)
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
    val v10 = Variable(cavaj.Type.Int, 10)
    val v11 = Variable(cavaj.Type.Int, 11)

    val bb0 = body.bbs(0).toList
    val expectedBB0 = List(
      Load(v3, v2),
      Load(v4, v1),
      Load(v10, Mul(v4, v3)),
      Load(v3, v10),
      Goto(1)
    )
    assertEquals(bb0, expectedBB0)

    val bb1 = body.bbs(1).toList
    val expectedBB1 = List(
      Load(v5, v1),
      Load(v6, v3),
      Br(CmpLe(v6, v5), 3, 2)
    )
    assertEquals(bb1, expectedBB1)

    val bb2 = body.bbs(2).toList
    val expectedBB2 = List(
      Load(v7, v2),
      Load(v8, v3),
      Load(v11, Sub(v8, v7)),
      Load(v3, v11),
      Goto(1)
    )
    assertEquals(bb2, expectedBB2)

    val bb3 = body.bbs(3).toList
    val expectedBB3 = List(
      Load(v9, v3),
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
    assertNoStackOps(irClass)
    assert(irClass.name == "IfNoElse")

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
      Load(v3, v2),
      Load(v4, v1),
      Br(CmpLe(v4, v3), 2, 1),
    )
    assertEquals(bb0, expectedBB0)

    val bb1 = body.bbs(1).toList
    val expectedBB1 = List(
      Load(v5, v1),
      Return(v5),
    )
    assertEquals(bb1, expectedBB1)

    val bb2 = body.bbs(2).toList
    val expectedBB2 = List(
      Load(v6, v2),
      Return(v6),
    )
    assertEquals(bb2, expectedBB2)
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
    assertNoStackOps(irClass)
    assert(irClass.name == "Ternary")

    val method = irClass.methods("ternary").head
    val body = method.body.get

    assertEquals(body.bbs.size, 4, "expected 4 BBs")

    val v1 = Variable(cavaj.Type.Boolean, 1)
    val v2 = Variable(cavaj.Type.Int, 2)
    val v3 = Variable(cavaj.Type.Int, 3)
    val v4 = Variable(cavaj.Type.Undef, 4)

    val bb0 = body.bbs(0).toList
    val expectedBB0 = List(
      Load(v2, v1),
      Br(CmpEq(v2, IntLit(0)), 2, 1),
    )
    assertEquals(bb0, expectedBB0)

    val bb1 = body.bbs(1).toList
    val expectedBB1 = List(
      Load(v4, ByteLit(10)),
      Goto(3),
    )
    assertEquals(bb1, expectedBB1)

    val bb2 = body.bbs(2).toList
    val expectedBB2 = List(
      Load(v4, ByteLit(20)),
      Goto(3),
    )
    assertEquals(bb2, expectedBB2)

    val bb3 = body.bbs(3).toList
    val expectedBB3 = List(
      Load(v3, v4),
      Return(v3)
    )
    assertEquals(bb3, expectedBB3)
  }

  test("cool jvm boolean logic") {
    val code =
      """
      |class CoolJVMBooleanLogic {
      |    boolean test(int a, int b) {
      |        return (a > 0 || b > 0) && (a < 100);
      |    }
      |}
      """.stripMargin

    val irClass = toClass("CoolJVMBooleanLogic", code)
    printCmp(code, irClass)
    assertNoStackOps(irClass)
    assert(irClass.name == "CoolJVMBooleanLogic")

    val method = irClass.methods("test").head
    val body = method.body.get

    assertEquals(body.bbs.size, 6, "expected 6 BBs")

    val v1 = Variable(cavaj.Type.Int, 1)
    val v2 = Variable(cavaj.Type.Int, 2)
    val v3 = Variable(cavaj.Type.Int, 3)
    val v4 = Variable(cavaj.Type.Int, 4)
    val v5 = Variable(cavaj.Type.Int, 5)
    val v6 = Variable(cavaj.Type.Int, 6)
    val v7 = Variable(cavaj.Type.Boolean, 7)
    val v8 = Variable(cavaj.Type.Undef, 8)

    val bb0 = body.bbs(0).toList
    val expectedBB0 = List(
      Load(v3, v1),
      Br(CmpGt(v3, IntLit(0)), 2, 1)
    )
    assertEquals(bb0, expectedBB0)

    val bb1 = body.bbs(1).toList
    val expectedBB1 = List(
      Load(v4, v2),
      Br(CmpLe(v4, IntLit(0)), 4, 2)
    )
    assertEquals(bb1, expectedBB1)

    val bb2 = body.bbs(2).toList
    val expectedBB2 = List(
      Load(v5, ByteLit(100)),
      Load(v6, v1),
      Br(CmpGe(v6, v5), 4, 3)
    )
    assertEquals(bb2, expectedBB2)

    val bb3 = body.bbs(3).toList
    val expectedBB3 = List(
      Load(v8, IntLit(1)),
      Goto(5)
    )
    assertEquals(bb3, expectedBB3)

    val bb4 = body.bbs(4).toList
    val expectedBB4 = List(
      Load(v8, IntLit(0)),
      Goto(5)
    )
    assertEquals(bb4, expectedBB4)

    val bb5 = body.bbs(5).toList
    val expectedBB5 = List(
      Load(v7, v8),
      Return(v7)
    )
    assertEquals(bb5, expectedBB5)
  }

  test("deep stack") {
    val code =
      """
      |class DeepStack {
      |    int f(int a, int b, int c, int d) {
      |        return calc(a + b, c + d, a * d);
      |    }
      |    int calc(int x, int y, int z) { return x + y + z; }
      |}
      """.stripMargin

    val irClass = toClass("DeepStack", code)
    printCmp(code, irClass)
    assertNoStackOps(irClass)

    assert(irClass.name == "DeepStack")

    val f = irClass.methods("f").head
    val f_body = f.body.get

    assertEquals(f_body.bbs.size, 1, "expected 1 BBs")

    val v0 = Variable(cavaj.Type.Int, 0)
    val v1 = Variable(cavaj.Type.Int, 1)
    val v2 = Variable(cavaj.Type.Int, 2)
    val v3 = Variable(cavaj.Type.Int, 3)
    val v4 = Variable(cavaj.Type.Int, 4)
    val v5 = Variable(cavaj.Type.Int, 5)
    val v6 = Variable(cavaj.Type.Int, 6)
    val v7 = Variable(cavaj.Type.Int, 7)
    val v8 = Variable(cavaj.Type.Int, 8)
    val v9 = Variable(cavaj.Type.Int, 9)
    val v10 = Variable(cavaj.Type.Int, 10)
    val v11 = Variable(cavaj.Type.Undef, 11)
    val v12 = Variable(cavaj.Type.Undef, 12)
    val v13 = Variable(cavaj.Type.Undef, 13)
    val v14 = Variable(cavaj.Type.Reference(), 14)
    val v15 = Variable(cavaj.Type.Int, 15)
    val v16 = Variable(cavaj.Type.Int, 16)
    val v17 = Variable(cavaj.Type.Int, 17)
    val v18 = Variable(cavaj.Type.Int, 18)
    val v19 = Variable(cavaj.Type.Int, 19)

    val bb0 = f_body.bbs(0).toList
    val expectedBB0 = List(
      Load(v5, v2),
      Load(v6, v1),
      Load(v16, Add(v6, v5)),
      Load(v7, v4),
      Load(v8, v3),
      Load(v17, Add(v8, v7)),
      Load(v9, v4),
      Load(v10, v1),
      Load(v18, Mul(v10, v9)),
      Load(v11, v18),
      Load(v12, v17),
      Load(v13, v16),
      Load(v14, v0),
      Load(v19, InvokeInstanceMethod(v14, "calc", List(v13, v12, v11), cavaj.Type.Int)),
      Load(v15, v19),
      Return(v15)
    )
//    assertEquals(bb0, expectedBB0)


    val calc = irClass.methods("calc").head
    val calc_body = calc.body.get

    assertEquals(calc_body.bbs.size, 1, "expected 1 BBs")
    val bb1 = calc_body.bbs(0).toList
    val expectedBB1 = List(
      Load(v4, v2),
      Load(v5, v1),
      Load(v9, Add(v5, v4)),
      Load(v6, v3),
      Load(v7, v9),
      Load(v10, Add(v7, v6)),
      Load(v8, v10),
      Return(v8)
    )
    assertEquals(bb1, expectedBB1)
  }

  test("recursive fibonacci") {
    val code =
      """
      |class Fib {
      |    int fib(int n) {
      |        if (n <= 1) return n;
      |        return fib(n - 1) + fib(n - 2);
      |    }
      |}
      """.stripMargin

    val irClass = toClass("Fib", code)
    printCmp(code, irClass)
    assertNoStackOps(irClass)

    assert(irClass.name == "Fib")

    val fib = irClass.methods("fib").head
    val body = fib.body.get

    assertEquals(body.bbs.size, 3, "expected 3 BBs")

    val v0 = Variable(cavaj.Type.Reference(), 0)
    val v1 = Variable(cavaj.Type.Int, 1)
    val v2 = Variable(cavaj.Type.Int, 2)
    val v3 = Variable(cavaj.Type.Int, 3)
    val v4 = Variable(cavaj.Type.Int, 4)
    val v5 = Variable(cavaj.Type.Int, 5)
    val v6 = Variable(cavaj.Type.Int, 6)
    val v7 = Variable(cavaj.Type.Undef, 7)
    val v8 = Variable(cavaj.Type.Reference("Fib"), 8)
    val v9 = Variable(cavaj.Type.Int, 9)
    val v10 = Variable(cavaj.Type.Int, 10)
    val v11 = Variable(cavaj.Type.Undef, 11)
    val v12 = Variable(cavaj.Type.Reference("Fib"), 12)
    val v13 = Variable(cavaj.Type.Undef, 13)
    val v14 = Variable(cavaj.Type.Int, 14)
    val v15 = Variable(cavaj.Type.Int, 15)
    val v16 = Variable(cavaj.Type.Int, 16)
    val v17 = Variable(cavaj.Type.Int, 17)
    val v18 = Variable(cavaj.Type.Int, 18)
    val v19 = Variable(cavaj.Type.Int, 19)
    val v20 = Variable(cavaj.Type.Int, 20)

    val bb0 = body.bbs(0).toList
    val expectedBB0 = List(
      Load(v2, IntLit(1)),
      Load(v3, v1),
      Br(CmpGt(v3, v2), 2, 1)
    )
    assertEquals(bb0, expectedBB0)

    val bb1 = body.bbs(1).toList
    val expectedBB1 = List(
      Load(v4, v1),
      Return(v4)
    )
    assertEquals(bb1, expectedBB1)

    val bb2 = body.bbs(2).toList
    val expectedBB2 = List(
      Load(v5, IntLit(1)),
      Load(v6, v1),
      Load(v16, Sub(v6, v5)),
      Load(v7, v16),
      Load(v8, v0),
      Load(v17, InvokeInstanceMethod(v8, "fib", List(v7), cavaj.Type.Int)),
      Load(v9, IntLit(2)),
      Load(v10, v1),
      Load(v18, Sub(v10, v9)),
      Load(v11, v18),
      Load(v12, v0),
      Load(v19, InvokeInstanceMethod(v12, "fib", List(v11), cavaj.Type.Int)),
      Load(v13, v19),
      Load(v14, v17),
      Load(v20, Add(v14, v13)),
      Load(v15, v20),
      Return(v15)
    )
    assertEquals(bb2, expectedBB2)
  }

  test("eager evaluation") {
    val code =
      """
      |class Eager {
      |    static int val = 0;
      |    static int getA() { val += 1; return val; }
      |    static int getB() { val += 10; return val; }
      |    static int test() {
      |        // getA() must be called before the getB()
      |        return getA() - getB(); 
      |    }
      |}
      """.stripMargin

    val irClass = toClass("Eager", code)
    printCmp(code, irClass)
    assertNoStackOps(irClass)

    assert(irClass.name == "Eager")

    val test = irClass.methods("test").head
    val body = test.body.get

    val v1 = Variable(cavaj.Type.Int, 1)
    val v2 = Variable(cavaj.Type.Int, 2)
    val v3 = Variable(cavaj.Type.Int, 3)
    val v4 = Variable(cavaj.Type.Int, 4)
    val v5 = Variable(cavaj.Type.Int, 5)

    assertEquals(body.bbs.size, 1, "expected 1 BBs")
    val bb0 = body.bbs(0).toList
    val expecetedBB0 = List(
      Load(v3, InvokeStaticMethod("Eager", "getA", List(), cavaj.Type.Int)),
      
    )
    assertEquals(bb0, expecetedBB0)
  }
}
