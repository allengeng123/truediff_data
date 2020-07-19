package truediff.trie

class ByteArrayPatriciaTrie[E] extends AbstractPatriciaTrie[Array[Byte], E](ByteArrayKeyAnalyzer)

object ByteArrayKeyAnalyzer extends KeyAnalyzer[Array[Byte]] {

  val leadingZeros: Array[Int] = Array(
    8,7,6,6,5,5,5,5,
    4,4,4,4,4,4,4,4,
    3,3,3,3,3,3,3,3,
    3,3,3,3,3,3,3,3,
    2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,
    2,2,2,2,2,2,2,2,
    1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,
    1,1,1,1,1,1,1,1,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0,
    0,0,0,0,0,0,0,0
  )

  /**
   * Returns the number of bits per element in the key.
   * This is only useful for variable-length keys, such as Strings.
   *
   * @return the number of bits per element
   */
  override def bitsPerElement() =
    8

  /**
   * Returns the length of the Key in bits.
   *
   * @param key the key
   * @return the bit length of the key
   */
  override def lengthInBits(key: Array[Byte]): Int =
    key.length * 8

  /**
   * Returns whether or not a bit is set.
   *
   * @param key          the key to check, may not be null
   * @param bitIndex     the bit index to check
   * @param lengthInBits the maximum key length in bits to check
   * @return { @code true} if the bit is set in the given key and
   *                 { @code bitIndex} &lt; { @code lengthInBits}, { @code false} otherwise.
   */
  override def isBitSet(key: Array[Byte], bitIndex: Int, lengthInBits: Int): Boolean = {
    val index = bitIndex / 8
    val bit = bitIndex % 8
    val mask = 256 >> bit
    val keyBit = key(index) & mask
    keyBit != 0
  }

  /**
   * Returns the n-th different bit between key and other. This starts the comparison in
   * key at 'offsetInBits' and goes for 'lengthInBits' bits, and compares to the other key starting
   * at 'otherOffsetInBits' and going for 'otherLengthInBits' bits.
   *
   * @param key               the key to use
   * @param offsetInBits      the bit offset in the key
   * @param lengthInBits      the maximum key length in bits to use
   * @param other             the other key to use
   * @param otherOffsetInBits the bit offset in the other key
   * @param otherLengthInBits the maximum key length in bits for the other key
   * @return the bit index where the key and other first differ
   */
  override def bitIndex(key: Array[Byte], offsetInBits: Int, lengthInBits: Int, other: Array[Byte], otherOffsetInBits: Int, otherLengthInBits: Int): Int = {

    if (offsetInBits % 8 != 0 || otherOffsetInBits % 8 != 0 || lengthInBits % 8 != 0 || otherLengthInBits % 8 != 0)
      throw new IllegalArgumentException("The offsets and lengths must be at Byte boundaries")

    var allNull = true

    val beginIndex1 = offsetInBits / 8
    val beginIndex2 = otherOffsetInBits / 8

    val endIndex1 = beginIndex1 + lengthInBits / 8
    val endIndex2 = beginIndex2 + otherLengthInBits / 8

    val length = Math.max(endIndex1, endIndex2)

    // Look at each byte, and if they're different
    // then figure out which bit makes the difference
    // and return it.
    var k: Byte = 0
    var f: Byte = 0
    for (i <- 0 until length) {
      val index1 = beginIndex1 + i
      val index2 = beginIndex2 + i
      if (index1 >= endIndex1)
        k = 0
      else
        k = key(index1)
      if (index2 >= endIndex2)
        f = 0
      else
        f = other(index2)
      if (k != f) {
        val x = k ^ f
        val zeros = leadingZeros(x & 0xff)
        val bit = i * 8 + zeros + 1
        return bit
      }
      if (k != 0)
        allNull = false
    }

    if (allNull) {
      // All bits are 0
      KeyAnalyzer.NULL_BIT_KEY
    } else {
      // Both keys are equal
      KeyAnalyzer.EQUAL_BIT_KEY
    }
  }

  /**
   * Determines whether or not the given prefix (from offset to length) is a prefix of the given key.
   *
   * @param prefix       the prefix to check
   * @param offsetInBits the bit offset in the key
   * @param lengthInBits the maximum key length in bits to use
   * @param key          the key to check
   * @return { @code true} if this is a valid prefix for the given key
   */
  override def isPrefix(prefix: Array[Byte], offsetInBits: Int, lengthInBits: Int, key: Array[Byte]): Boolean = {
    if (offsetInBits % 8 != 0 || lengthInBits % 8 != 0)
      throw new IllegalArgumentException("Cannot determine prefix outside of Byte boundaries")

    val offset = offsetInBits / 8
    val length = lengthInBits / 8

    var ix = offset
    while (ix < length) {
      if (prefix(ix) != key(ix))
        return false
      ix += 1
    }
    true
  }

  override def compare(o1: Array[Byte], o2: Array[Byte]): Int = {
    for (i <- 0 until Math.min(o1.length, o2.length)) {
      val compared = o1(i) - o2(i) // as in java.lang.Byte.compare
      if (compared != 0)
        return compared
    }
    o1.length - o2.length
  }

}
