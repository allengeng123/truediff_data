package truediff.macros

import truediff.changeset.{ChangesetBuffer, DetachNode, LoadNode, UnloadNode}
import truediff.diffable.Diffable
import truediff.{Hashable, Link, NamedLink, NodeURI}

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@compileTimeOnly("Scala 2.13 and compiler flag -Ymacro-annotations required")
class diffable extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DiffableMacro.impl
}

object DiffableMacro {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    import Util._
    import c.universe._

    val tDiffable = symbolOf[Diffable]
    val tyDiffable = typeOf[Diffable]
    val tHashable = symbolOf[Hashable]
    val oHashable = tHashable.companion
    val tArray = symbolOf[Array[_]]
    val tVector = symbolOf[scala.collection.immutable.Vector[_]]
    val oVector = tVector.companion
    val tByte = symbolOf[Byte]
    val oBigInt = symbolOf[BigInt.type].asClass.module
    val oSeq = symbolOf[Seq.type].asClass.module
    val oMath = symbolOf[Math].companion
    val tNodeURI = symbolOf[NodeURI]

    val oLiteral = symbolOf[truediff.Literal[_]].companion
    val tLink = symbolOf[Link]
    val oNamedLink = symbolOf[NamedLink].companion
    val oLoadNode = symbolOf[LoadNode].companion
    val oDetachNode = symbolOf[DetachNode].companion
    val oUnloadNode = symbolOf[UnloadNode].companion

    val tChangesetBuffer = symbolOf[ChangesetBuffer]

    val annoDiffable = q"new _root_.truediff.macros.diffable()"
    val name = nameOf(c)(annottees.head)

    def rewrite(t: Tree): Tree = t match {
      case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents with $tDiffable { $self => ..$stats }"

      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>

        val oThis = TermName(tpname.toString)

        def mapAllParams[A](diffable: TermName => A, nonDiffable: TermName => A, option: TermName => A, seq: TermName => A): Seq[A] =
          mapParams(c)(paramss, tyDiffable, diffable, nonDiffable, option, seq)

        def mapDiffableParams(diffable: TermName => Tree, option: TermName => Tree, seq: TermName => Tree): Seq[Tree] =
          mapParams(c)(paramss, tyDiffable, p => Some(diffable(p)), _ => None, p => Some(option(p)), p => Some(seq(p))).flatten

        def mapNonDiffableParams(nonDiffable: TermName => Tree): Seq[Tree] =
          mapParams(c)(paramss, tyDiffable, _ => None, p => Some(nonDiffable(p)), _ => None, _ => None).flatten

        def nondiffableCond(other: Tree) =
          reduceInfix(c)(mapNonDiffableParams(p => q"this.$p == $other.$p"), TermName("$amp$amp"), q"")

        q"""
          $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents with $tDiffable { $self =>
            ..$stats

            override def toStringWithURI: String = {
                val paramStrings = $oSeq(..${mapParams(c)(paramss, tyDiffable,
                  p => q"this.$p.toStringWithURI",
                  p => q"this.$p.toString",
                  p => q"this.$p.map(_.toStringWithURI).toString",
                  p => q"this.$p.map(_.toStringWithURI).toString")
                })
                $oThis + "_" + this.uri.toString + paramStrings.mkString("(", ",", ")")
              }

            override lazy val hash: $tArray[$tByte] = {
              val digest = $oHashable.mkDigest
              digest.update(this.getClass.getCanonicalName.getBytes)
              ..${Util.mapParams(c)(paramss, tyDiffable,
                p => q"digest.update(this.$p.hash)",
                p => q"$oHashable.hash(this.$p, digest)",
                p => q"{if ($p.isEmpty) digest.update(0:$tByte) else {digest.update(1:$tByte); digest.update($p.get)}}",
                p => q"{digest.update($oBigInt($p.size).toByteArray); $p.foreach((x: $tDiffable) => digest.update(x.hash))}"
              )}
              digest.digest()
            }

            override val height: Int = 1 + ${
              reducePrefix(c)(
                mapDiffableParams(
                  p => q"this.$p.height",
                  p => q"this.$p.map(_.height).getOrElse(0)",
                  p => q"this.$p.foldLeft(0)((max, s) => $oMath.max(max, s.height))"
                ),
                q"$oMath.max",
                q"0")
            }

            override private[truediff] def diffableKids: $tVector[$tDiffable] = $oVector(..${mapDiffableParams(
              p => q"this.$p",
              p => ???,
              p => ???
            )})

            override private[truediff] def computeChangesetRecurse(that: $tDiffable, parent: $tNodeURI, link: $tLink, changes: $tChangesetBuffer): $tDiffable = that match {
              case that: $tpname if ${nondiffableCond(q"that")} =>
                ..${mapParamsTyped(c)(paramss, tyDiffable,
                  (p,t) => q"val $p = this.$p.computeChangeset(that.$p, this.uri, $oNamedLink(${p.toString}), changes).asInstanceOf[$t]",
                  (p,t) => q"val $p = this.$p",
                  (p,t) => ???,
                  (p,t) => ???
                )}
                val $$newtree = $oThis(..${mapParams(c)(paramss, tyDiffable, p => q"$p", p => q"$p", p => q"$p", p => q"$p")})
                $$newtree._uri = this.uri
                $$newtree
              case _ => null
            }

            override def loadUnassigned(changes: $tChangesetBuffer): $tDiffable = {
              val that = this
              if (that.assigned != null) {
                return that.assigned
              }

              ..${mapParamsTyped(c)(paramss, tyDiffable,
                (p,t) => q"val $p = that.$p.loadUnassigned(changes).asInstanceOf[$t]",
                (p,t) => q"val $p = that.$p",
                (p,t) => ???,
                (p,t) => ???
              )}
              val $$newtree = $oThis(..${mapParams(c)(paramss, tyDiffable, p => q"$p", p => q"$p", p => q"$p", p => q"$p")})
              changes += $oLoadNode($$newtree.uri, this.tag, $oSeq(
                ..${mapParams(c)(paramss, tyDiffable,
                  p => q"($oNamedLink(${p.toString}), $p.uri)",
                  p => q"($oNamedLink(${p.toString}), $oLiteral($p))",
                  p => ???,
                  p => ???
                )}
              ))
              $$newtree
            }

            override def unloadUnassigned(parent: $tNodeURI, link: $tLink, changes: $tChangesetBuffer): Unit = {
              if (this.assigned != null) {
                changes += $oDetachNode(parent, link, this.uri)
                this.assigned = null
              } else {
                ..${mapDiffableParams(
                  p => q"this.$p.unloadUnassigned(this.uri, $oNamedLink(${p.toString}), changes)",
                  p => ???,
                  p => ???
                )}
                changes += $oUnloadNode(parent, link, this.uri, $oSeq(
                  ..${mapDiffableParams(p => q"$oNamedLink(${p.toString})", p => q"$oNamedLink(${p.toString})", p => q"$oNamedLink(${p.toString})")}
                ))
              }
            }

          }
         """

      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
        q"""
          $mods object $tname extends { ..$earlydefns } with ..$parents  { $self =>
            ..${body.map(b => Util.addAnnotation(c)(b, annoDiffable, _ => true))}
          }
         """
    }

    val res = q"{..${annottees.map(a => rewrite(a))}}"

    println(res)
    res
  }
}