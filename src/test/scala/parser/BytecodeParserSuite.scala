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

    // TODO

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

    // TODO

    assert(irClass.name == "Local")
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

    // TODO

    assert(irClass.name == "Basic")
    assertEquals(irClass.methods.values.flatten.size, 7) // 5 methods + 2 constructors
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

    // TODO

    assert(irClass.name == "Not")
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

    // TODO

    assert(irClass.name == "Ternary")
  }

}
