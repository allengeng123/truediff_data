package truechange

import scala.collection.mutable

class EditScriptBuffer() {
    private val negBuf: mutable.ArrayBuffer[Edit] = mutable.ArrayBuffer()
    private val posBuf: mutable.ArrayBuffer[Edit] = mutable.ArrayBuffer()

    private val detachListNext: mutable.Set[(URI, URI)] = mutable.Set()
    private val attachListNext: mutable.Set[(URI, URI)] = mutable.Set()

    def lastNegOption: Option[Edit] = negBuf.lastOption
    def dropLastNeg(): Unit = negBuf.dropRightInPlace(1)
    def lastPosOption: Option[Edit] = posBuf.lastOption
    def dropLastPos(): Unit = posBuf.dropRightInPlace(1)

    def += (elem: Edit): this.type = {
      elem match {
        case Detach(node, tag, link, parent, ptag) => link match {
          case next: ListNextLink =>
            val nextPair = parent -> node
            if (attachListNext.remove(nextPair)) {
              // detach cancelled out previous attach
              posBuf -= Attach(node, tag, next, parent, ptag)
            } else {
              detachListNext += nextPair
              negBuf += elem
            }
          case _ =>
            negBuf += elem
        }

        case _: Unload | _: Remove =>
          negBuf += elem

        case _: Load | _: Update | _: Insert =>
          posBuf += elem

        case Attach(node, tag, link, parent, ptag) => link match {
            case next: ListNextLink =>
              val nextPair = (parent, node)
              if (detachListNext.remove(nextPair)) {
                // attach cancelled out previous detach
                negBuf -= Detach(node, tag, next, parent, ptag)
              } else {
                attachListNext += nextPair
                posBuf += elem
              }
            case _ =>
              posBuf += elem
        }
      }

      this
    }

    def ++= (es: IterableOnce[Edit]): this.type = {
      for (e <- es)
        this += e
      this
    }

    def mergeKidInsert(node: URI): Either[Insert, URI] = {
      lastPosOption match {
        case Some(i: Insert) if i.node == node =>
          dropLastPos()
          Left(i)
        case _ =>
          Right(node)
      }
    }

    def mergeKidRemove(node: URI, link: String): Unit = {
      lastNegOption match {
        case Some(r: Remove) if r.node == node =>
          dropLastNeg()
          lastNegOption match {
            case Some(remove: RemoveNode) =>
              this.dropLastNeg()
              val kidRemoves = remove.kids + (link -> Left(r))
              this += remove.copy(kids = kidRemoves)
            case _ =>
              // undo
              negBuf += r
          }
        case _ =>
          // nothing
      }
    }

    def toEditScript: EditScript = EditScript(negBuf.toSeq ++ posBuf.toSeq)
  }