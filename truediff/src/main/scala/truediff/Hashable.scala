package truediff

import java.security.MessageDigest
import java.util.Base64

trait Hashable {
  val structureHash: Array[Byte]
  lazy val structureHashString: String = Base64.getEncoder.encodeToString(this.structureHash)

  val literalHash: Array[Byte]
  lazy val literalHashString: String = Base64.getEncoder.encodeToString(this.literalHash)

  def isLiterallyEqual(that: Hashable): Boolean =
    this.literalHashString == that.literalHashString
}

object Hashable {
  def mkDigest: MessageDigest = MessageDigest.getInstance("SHA-256")

  def hash(vs: Any*): Array[Byte] = {
    val digest = mkDigest
    vs.foreach(hash(_, digest))
    digest.digest()
  }

  def hash(v: Any, d: MessageDigest): Unit = {
    d.update(v.getClass.getCanonicalName.getBytes)
    v match {
      case v: Boolean => d.update(if(v) 1:Byte else 0:Byte)
      case v: Byte => d.update(v)
      case v: Short  => d.update(intToBytes(v))
      case v: Char  => d.update(intToBytes(v))
      case v: Int => d.update(intToBytes(v))
      case v: Long => d.update(longToBytes(v))
      case v: Float => d.update(floatToBytes(v))
      case v: Double => d.update(doubleToBytes(v))
      case v: String => d.update(v.getBytes)
      case v: Symbol => d.update(v.name.getBytes)
      case v: BigInt => d.update(v.toByteArray)
      case v: BigDecimal => d.update(v.bigDecimal.unscaledValue().toByteArray); d.update(intToBytes(v.scale))
      case None => d.update(0.toByte)
      case Some(v) => d.update(1.toByte); hash(v, d)
      case seq: Seq[_] => hash(seq.size, d); seq.foreach(hash(_, d))
      case prod: Product => prod.productIterator.foreach(hash(_, d))
      case _ => throw new IllegalArgumentException(s"Cannot compute hash of $v (class ${v.getClass})")
    }
  }

  private def intToBytes(data: Int) = Array[Byte](
    ((data >> 24) & 0xff).toByte,
    ((data >> 16) & 0xff).toByte,
    ((data >> 8) & 0xff).toByte,
    ((data >> 0) & 0xff).toByte)

  private def longToBytes(data: Long) = Array[Byte](
    ((data >> 56) & 0xff).toByte,
    ((data >> 48) & 0xff).toByte,
    ((data >> 40) & 0xff).toByte,
    ((data >> 32) & 0xff).toByte,
    ((data >> 24) & 0xff).toByte,
    ((data >> 16) & 0xff).toByte,
    ((data >> 8) & 0xff).toByte,
    ((data >> 0) & 0xff).toByte)

  private def floatToBytes(data: Float) = {
    import java.nio.ByteBuffer
    val bytes = new Array[Byte](4)
    ByteBuffer.wrap(bytes).putFloat(data)
    bytes
  }

  private def doubleToBytes(data: Double) = {
    import java.nio.ByteBuffer
    val bytes = new Array[Byte](8)
    ByteBuffer.wrap(bytes).putDouble(data)
    bytes
  }
}
