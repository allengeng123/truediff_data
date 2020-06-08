package truechange

import scala.collection.mutable

class ChangesetBuffer() {
    private val negBuf: mutable.Buffer[NegativeChange] = mutable.ArrayBuffer()
    private val posBuf: mutable.Buffer[PositiveChange] = mutable.ArrayBuffer()
    private val detachListNext: mutable.Map[(NodeURI, NodeURI), DetachNode] = mutable.Map()
    private val attachListNext: mutable.Set[(NodeURI, NodeURI)] = mutable.Set()

    def += (elem: Change): this.type = {
      elem match {
        case detach@DetachNode(pred, next@ListNextLink(_), node, _) =>
          val nextPair = (pred, node)
          if (attachListNext.remove(nextPair)) {
            // detach cancelled out previous attach
            posBuf -= AttachNode(pred, next, node)
          } else {
            detachListNext += ((nextPair, detach))
            negBuf += detach
          }
        case attach@AttachNode(pred, ListNextLink(_), node) =>
          val nextPair = (pred, node)
          detachListNext.remove(nextPair) match {
            case Some(detach) =>
              // attach cancelled out previous detach
              negBuf -= detach
            case None =>
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