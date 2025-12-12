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

    val pkg  = Package(Map.empty, Map(name -> irClass))
    val pass = new StackElimination()
    pass.run(pkg).classes(name)
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
  }

  test("lots of local variables") {
    val code =
      """
      |class Local {
      |    int f(int a, int b, int c) {
      |        int tt0 = 12;
      |        int tt1 = 13;
      |        int t1 = tt0 + tt1;
      |        int t2 = t1 * tt0 + tt1;
      |        return (a << t1) & (b >> t2) | (c + t1 + t2);
      |    }
      |}
      """.stripMargin

    val irClass = toClass("Local", code)
    printCmp(code, irClass)

    assertNoStackOps(irClass)
  }

  test("complex class") {
    val code =
      """
      |class Basic {
      |    private boolean state = false;
      |    private static String greeting = "HELLO FINLAND";
      |
      |    private static String getTrueGreeting() {
      |        return "True greeting.";
      |    }
      |
      |    private static String getFalseGreeting() {
      |        return "False greeting.";
      |    }
      |
      |    public void greet() {
      |        System.out.println(greeting);
      |    }
      |
      |    public void printGreeting() {
      |        String s = new String();
      |        if (state) {
      |            s = getTrueGreeting();
      |        } else {
      |            s = getFalseGreeting();
      |        }
      |        System.out.println(s);
      |    }
      |
      |    public static void main(String[] args) {
      |        Basic b = new Basic();
      |        b.printGreeting();
      |        b.printGreeting();
      |        b.greet();
      |    }
      |}
      """.stripMargin

    val irClass = toClass("Basic", code)
    printCmp(code, irClass)

    assertNoStackOps(irClass)
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

  }
}
