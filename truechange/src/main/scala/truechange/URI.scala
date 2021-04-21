package truechange

import java.io.{ByteArrayOutputStream, ObjectOutputStream}

@SerialVersionUID(100L)
trait URI extends Serializable {
  def serialized: Array[Byte] = {
    val byteStream = new ByteArrayOutputStream()
    val stream = new ObjectOutputStream(byteStream)
    try {
      stream.writeObject(this)
      byteStream.toByteArray
    } finally {
      stream.close()
    }
  }

}

@SerialVersionUID(100L)
class JVMURI extends URI {
  override def toString: String = {
    val s = super.toString
    s.substring(s.lastIndexOf('@')+1)
  }
}


