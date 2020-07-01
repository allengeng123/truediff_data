package truechange

import scala.collection.mutable

class EditscriptBuffer() {
    private val negBuf: mutable.Buffer[Edit] = mutable.ArrayBuffer()
    private val posBuf: mutable.Buffer[Edit] = mutable.ArrayBuffer()

    private val detachListNext: mutable.Set[(URI, URI)] = mutable.Set()
    private val attachListNext: mutable.Set[(URI, URI)] = mutable.Set()

    def += (elem: Edit): this.type = {
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

    def ++= (elem: IterableOnce[Edit]): this.type = {
      elem.iterator.foreach(this += _)
      this
    }

    def toEditscript: EditScript = new EditScript(negBuf.toSeq ++ posBuf.toSeq)
  }