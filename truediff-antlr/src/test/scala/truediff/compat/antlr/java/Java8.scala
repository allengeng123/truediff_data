package truediff.compat.antlr.java

import org.antlr.v4.runtime.{CharStreams, CommonTokenStream}
import truediff.compat.antlr.java.parser.{Java8Lexer, Java8Parser}

object Java8 {
  def parseCompilationUnit(resourcePath: String): Java8Parser.CompilationUnitContext = {
    val resource = this.getClass.getClassLoader.getResourceAsStream(resourcePath)
    val lexer = new Java8Lexer(CharStreams.fromStream(resource))
    val tokens = new CommonTokenStream(lexer)
    val parser = new Java8Parser(tokens)
    parser.setBuildParseTree(true)
    parser.compilationUnit()
  }


}
