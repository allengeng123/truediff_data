package truechange

import scala.collection.mutable

class ChangesetBuffer() {
    private val negBuf: mutable.Buffer[Change] = mutable.ArrayBuffer()
    private val posBuf: mutable.Buffer[Change] = mutable.ArrayBuffer()

    private val detachListNext: mutable.Set[(NodeURI, NodeURI)] = mutable.Set()
    private val attachListNext: mutable.Set[(NodeURI, NodeURI)] = mutable.Set()

    def += (elem: Change): this.type = {
      elem match {
        case detach@Detach(node, tag, next@ListNextLink(_), pred, predTag) =>
          val nextPair = (pred, node)
          if (attachListNext.remove(nextPair)) {
            // detach cancelled out previous attach
            posBuf -= Attach(node, tag, next, pred, predTag)
          } else {
            detachListNext += nextPair
            negBuf += detach
          }
        case attach@Attach(node, tag, next@ListNextLink(_), pred, predTag) =>
          val nextPair = (pred, node)
          if (detachListNext.remove(nextPair)) {
            // attach cancelled out previous detach
            negBuf -= Detach(node, tag, next, pred, predTag)
          } else {
            attachListNext += nextPair
            posBuf += attach
          }

        case _: Detach | _: Unload => negBuf += elem
        case _: Attach | _: Load => posBuf += elem
      }
      this
    }

    def ++= (elem: IterableOnce[Change]): this.type = {
      elem.iterator.foreach(this += _)
      this
    }

    def toChangeset: Changeset = new Changeset(negBuf.toSeq ++ posBuf.toSeq)
  }