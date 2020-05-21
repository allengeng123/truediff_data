package truediff.changeset

import scala.collection.mutable

class ChangesetBuffer() {
    private val negBuf: mutable.Buffer[NegativeChange] = mutable.ArrayBuffer()
    private val posBuf: mutable.Buffer[PositiveChange] = mutable.ArrayBuffer()
    
    def += (elem: Change): this.type = {
      elem match {
        case change: NegativeChange => negBuf += change
        case change: PositiveChange => posBuf += change
      }
      this
    }

    def ++= (elem: IterableOnce[Change]): this.type = {
      elem.iterator.foreach(this += _)
      this
    }

    def toChangeset: Changeset = new Changeset(negBuf.toSeq, posBuf.toSeq)
  }