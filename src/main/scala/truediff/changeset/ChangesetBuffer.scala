package truediff.changeset

import truediff.{ListNextLink, NodeURI}

import scala.collection.mutable

class ChangesetBuffer() {
    private val negBuf: mutable.Buffer[NegativeChange] = mutable.ArrayBuffer()
    private val posBuf: mutable.Buffer[PositiveChange] = mutable.ArrayBuffer()
    private val detachListNext: mutable.Set[(NodeURI, NodeURI)] = mutable.Set()
    private val attachListNext: mutable.Set[(NodeURI, NodeURI)] = mutable.Set()

    def += (elem: Change): this.type = {
      elem match {
        case detach@DetachNode(pred, ListNextLink, node) =>
          val nextPair = (pred, node)
          if (attachListNext.remove(nextPair)) {
            // detach cancelled out previous attach
            posBuf -= AttachNode(pred, ListNextLink, node)
          } else {
            detachListNext += nextPair
            negBuf += detach
          }
        case attach@AttachNode(pred, ListNextLink, node) =>
          val nextPair = (pred, node)
          if (detachListNext.remove(nextPair)) {
            negBuf -= DetachNode(pred, ListNextLink, node)
            // attach cancelled out previous detach
          } else {
            attachListNext += nextPair
            posBuf += attach
          }

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