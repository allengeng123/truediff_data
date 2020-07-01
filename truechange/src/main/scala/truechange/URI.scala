package truechange

trait URI

class JVMURI extends URI {
  override def toString: String = {
    val s = super.toString
    s.substring(s.lastIndexOf('@')+1)
  }
}




