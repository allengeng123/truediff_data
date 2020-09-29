package truediff.compat.treesitter

import scala.io.Source

trait TSLanguage {

  val langName: String
  def funName: String = langName.replace('-', '_')
  val dependencies: Seq[String] = Seq()

  protected def loadLang(): TSLanguagePointer = {
    val langFun = NativeLoad.loadFunction(langName, funName, classOf[TreeSitterLibrary], dependencies:_*)
    langFun.invoke(classOf[TSLanguagePointer], Array()).asInstanceOf[TSLanguagePointer]
  }

  private lazy val lang: TSLanguagePointer = loadLang()

  private lazy val tokenNodes: Set[String] = {
    Source.fromResource(s"$langName/token-nodes").getLines().toSet
  }

  def newParser(): TSParser = new TSParser(lang, tokenNodes)
}
