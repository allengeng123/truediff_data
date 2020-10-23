package truechange

import scala.collection.mutable

class EditScriptBuffer() {
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

        case _: Detach => negBuf += elem
        case Unload(node, tag, kids, lits) =>
          negBuf.lastOption match {
            case Some(Detach(`node`, _, link, parent, ptag)) =>
              negBuf.remove(negBuf.size - 1)
              negBuf += DetachUnload(node, tag, kids, lits, link, parent, ptag)
            case _ =>
              negBuf += elem
          }

        case _: Load => posBuf += elem
        case Attach(node, tag, link, parent, ptag) =>
          posBuf.lastOption match {
            case Some(Load(`node`, _, kids, lits)) =>
              posBuf.remove(posBuf.size - 1)
              posBuf += LoadAttach(node, tag, kids, lits, link, parent, ptag)
            case _ =>
              posBuf += elem
          }
      }
      this
    }

    def ++= (elem: IterableOnce[Edit]): this.type = {
      elem.iterator.foreach(this += _)
      this
    }

    def toEditScript: EditScript = new EditScript(negBuf.toSeq ++ posBuf.toSeq)
  }