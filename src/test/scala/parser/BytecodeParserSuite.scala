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

  private def askAssert: Unit =
    println("press Enter if the output is correct")
    assertEquals(readLine(), "")

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
    println(code)
    println(irClass)
    askAssert

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
    println(code)
    println(irClass)
    askAssert

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
    println(code)
    println(irClass)
    askAssert

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
    println(code)
    println(irClass)
    askAssert
  }

  test("release-stage feature example") {
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
    println(code)
    println(irClass)
    askAssert

    // TODO

    assert(irClass.name == "Not")
  }
}
