package cavaj

import ir.*
import analysis.*
import analysis.passes.*
import parser.BytecodeParser

val irTransforms = StackElimination andThen RestoreControlFlow

@main def main(args: String*): Unit = {
  for file <- args do {
    val c = BytecodeParser.parseClassFile(file)
    val ast = irTransforms.run(c)
    println(translator.translateClass(ast))
  }
}
