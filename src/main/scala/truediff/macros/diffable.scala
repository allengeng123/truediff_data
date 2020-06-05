package truediff.macros

import truediff._
import truediff.changeset.{ChangesetBuffer, DetachNode, LoadNode, UnloadNode}
import truediff.diffable._

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@compileTimeOnly("Scala 2.13 and compiler flag -Ymacro-annotations required")
class diffable extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro DiffableMacro.impl
}

object DiffableMacro {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val tDiffable = symbolOf[Diffable]
    val tyDiffable = typeOf[Diffable]
    val tHashable = symbolOf[Hashable]
    val oHashable = tHashable.companion
    val tUnit = symbolOf[Unit]
    val tInt = symbolOf[Int]
    val tArray = symbolOf[Array[_]]
    val oArray = tArray.companion
    val tByte = symbolOf[Byte]
    val oBigInt = symbolOf[BigInt.type].asClass.module
    val oSeq = symbolOf[Seq.type].asClass.module
    val oMath = symbolOf[Math].companion
    val tNodeURI = symbolOf[NodeURI]
    val tSubtreeRegistry = symbolOf[SubtreeRegistry]
    val tDiffableOption = symbolOf[DiffableOption[_]]
    val oDiffableOption = tDiffableOption.companion
    val tDiffableList = symbolOf[DiffableList[_]]
    val oDiffableList = tDiffableList.companion
    val tOption = symbolOf[Option[_]]
    val tSeq = symbolOf[Seq[_]]
    val tIterable = symbolOf[Iterable[_]]
    val oIterable = symbolOf[Iterable.type].asClass.module

    val oLiteral = symbolOf[truediff.Literal[_]].companion
    val tLink = symbolOf[Link]
    val oNamedLink = symbolOf[NamedLink].companion
    val oLoadNode = symbolOf[LoadNode].companion
    val oDetachNode = symbolOf[DetachNode].companion
    val oUnloadNode = symbolOf[UnloadNode].companion

    val tChangesetBuffer = symbolOf[ChangesetBuffer]

    val oOptionType = symbolOf[OptionType].companion
    val oListType = symbolOf[ListType].companion
    val oSortType = symbolOf[SortType].companion
    val tSignature = symbolOf[Signature]
    val oSignature = tSignature.companion

    val annoDiffable = q"new _root_.truediff.macros.diffable()"
    val name = Util.nameOf(c)(annottees.head)

    var hasCollectionParam: Boolean = false
    var classParamss: Seq[Tree] = null
    var classTp: Tree = null
    var oThis: TermName = null

    def rewrite(t: Tree): Tree = t match {
      case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        val newparents =
          if (parents.exists(tp => Util.isSubtypeOf(c)(tp, tyDiffable)))
            parents
          else
            parents :+ tq"$tDiffable"
        q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$newparents { $self => ..$stats }"



      case q"$mods class $tpname[..$tparams] $ctorMods(...$theparamss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>

        val paramss: Seq[Seq[Tree]] = theparamss.asInstanceOf[Seq[Seq[Tree]]].map(_.map(p => rewriteParam(p)))
        classParamss = theparamss.flatten
        classTp = tq"$tpname[..$tparams]"

        oThis = TermName(tpname.toString)

        val wat = (p: TermName) => throw new UnsupportedOperationException(s"parameter $p of $tpname")
        val watt = (p: TermName,_:Tree) => throw new UnsupportedOperationException(s"parameter $p of $tpname")

        def mapAllParams[A](diffable: TermName => A, nonDiffable: TermName => A): Seq[A] =
          Util.mapParams(c)(paramss, tyDiffable, diffable, nonDiffable, wat, wat)
        def mapAllParamsTyed[A](diffable: (TermName,Tree) => A, nonDiffable: (TermName,Tree) => A): Seq[A] =
          Util.mapParamsTyped(c)(paramss, tyDiffable, diffable, nonDiffable, watt, watt)

        def mapDiffableParams[A](diffable: TermName => A): Seq[A] =
          Util.mapParams(c)(paramss, tyDiffable, p => Some(diffable(p)), _ => None, wat, wat).flatten
        def mapDiffableParamsTyped[A](diffable: (TermName,Tree) => A): Seq[A] =
          Util.mapParamsTyped(c)(paramss, tyDiffable, (p,t) => Some(diffable(p,t)), (_,_) => None, watt, watt).flatten

        def mapNonDiffableParams[A](nonDiffable: TermName => A): Seq[A] =
          Util.mapParams(c)(paramss, tyDiffable, _ => None, p => Some(nonDiffable(p)), wat, wat).flatten
        def mapNonDiffableParamsTyped[A](nonDiffable: (TermName,Tree) => A): Seq[A] =
          Util.mapParamsTyped(c)(paramss, tyDiffable, (_,_) => None, (p,t) => Some(nonDiffable(p,t)), watt, watt).flatten

        def nondiffableCond(other: Tree) =
          Util.reduceInfix(c)(mapNonDiffableParams(p => q"this.$p == $other.$p"), TermName("$amp$amp"), q"")

        val diffableParams: Seq[TermName] = mapDiffableParams(p=>p)

        def link(p: TermName, tp: Tree) = q"$oNamedLink(this.tag, ${p.toString})"

        val superDiffable = parents.find(tp => Util.isSubtypeOf(c)(tp, tyDiffable))
        val sort = if (superDiffable.isDefined) asType(superDiffable.get) else q"$oSortType(classOf[$tpname[..$tparams]])"
        val newparents =
          if (superDiffable.isDefined)
            parents
          else
            parents :+ tq"$tDiffable"


        q"""
          $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$newparents { $self =>
            ..$stats

            override def toStringWithURI: String = {
                val paramStrings = $oSeq(..${mapAllParams(
                  p => q"this.$p.toStringWithURI",
                  p => q"this.$p.toString"
                )})
                this.tag + "_" + this.uri.toString + paramStrings.mkString("(", ",", ")")
              }

            override lazy val hash: $tArray[$tByte] = {
              val digest = $oHashable.mkDigest
              digest.update(this.getClass.getCanonicalName.getBytes)
              ..${mapAllParams(
                p => q"digest.update(this.$p.hash)",
                p => q"$oHashable.hash(this.$p, digest)"
              )}
              digest.digest()
            }

            override val treeheight: $tInt = 1 + ${
              Util.reducePrefix(c)(
                mapDiffableParams(p => q"this.$p.treeheight"),
                q"$oMath.max",
                q"0")
            }

            override lazy val treesize: $tInt =
                1 + ${
                  Util.reduceInfix(c)(
                    mapDiffableParams(p => q"this.$p.treesize"),
                    "$plus",
                    q"0")
                }

            override def sig: $tSignature = $oSignature($sort, this.tag,
                Map(..${mapDiffableParamsTyped((p,t) => q"(${p.toString}, ${asType(t)})")}),
                Map(..${mapNonDiffableParamsTyped((p,t) => q"(${p.toString}, classOf[$t])")})
            )

            override private[truediff] def foreachDiffable(f: $tDiffable => $tUnit): $tUnit = {
              f(this)
              ..${
                mapDiffableParams(p => q"this.$p.foreachDiffable(f)")}
            }

            override private[truediff] def assignSharesRecurse(that: $tDiffable, subtreeReg: $tSubtreeRegistry): $tUnit = that match {
              case that: $tpname if ${nondiffableCond(q"that")} =>
                ..${mapDiffableParams(p => q"this.$p.assignShares(that.$p, subtreeReg)")}
              case _ =>
                ..${mapDiffableParams(p => q"this.$p.foreachDiffable(subtreeReg.registerShareFor)")}
                that.foreachDiffable(subtreeReg.shareFor)
            }

            override private[truediff] def assignSubtreesRecurse(): $tIterable[$tDiffable] =
              ${diffableParams match {
                case Seq() =>
                  q"$oIterable.empty"
                case Seq(p1) =>
                  q"$oIterable.single(this.$p1)"
                case ps =>
                  q"$oIterable(..${ps.map(p => q"this.$p")})"
              }}

            override private[truediff] def computeChangesetRecurse(that: $tDiffable, parent: $tNodeURI, link: $tLink, changes: $tChangesetBuffer): $tDiffable = that match {
              case that: $tpname if ${nondiffableCond(q"that")} =>
                ..${mapAllParamsTyed(
                  (p,t) => q"val $p = this.$p.computeChangeset(that.$p, this.uri, ${link(p, t)}, changes).asInstanceOf[$t]",
                  (p,t) => q"val $p = this.$p"
                )}
                val $$newtree = $oThis(..${mapAllParams(p => q"$p", p => q"$p")})
                $$newtree._uri = this.uri
                $$newtree
              case _ => null
            }

            override def loadUnassigned(changes: $tChangesetBuffer): $tDiffable = {
              val that = this
              if (that.assigned != null) {
                return that.assigned
              }

              ..${mapAllParamsTyed(
                (p,t) => q"val $p = that.$p.loadUnassigned(changes).asInstanceOf[$t]",
                (p,t) => q"val $p = that.$p"
              )}
              val $$newtree = $oThis(..${mapAllParams(p => q"$p", p => q"$p")})
              changes += $oLoadNode($$newtree.uri, this.tag,
                $oSeq(..${mapDiffableParamsTyped((p,t) => q"(${p.toString}, $p.uri)")}),
                $oSeq(..${mapNonDiffableParams(p => q"(${p.toString}, $oLiteral($p))")}),
              )
              $$newtree
            }

            override def unloadUnassigned(parent: $tNodeURI, link: $tLink, changes: $tChangesetBuffer): $tUnit = {
              if (this.assigned != null) {
                changes += $oDetachNode(parent, link, this.uri, this.tag)
                this.assigned = null
              } else {
                ..${mapDiffableParamsTyped(
                  (p,t) => q"this.$p.unloadUnassigned(this.uri, ${link(p,t)}, changes)"
                )}
                changes += $oUnloadNode(parent, link, this.uri, this.tag)
              }
            }

          }
         """

      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
        if (parents.exists(tp => Util.isSubtypeOf(c)(tp, tyDiffable)))
          throw new IllegalArgumentException(s"Cannot generate diffable code for objects. Consider making ${tname.toString} a class instead.")
        q"""
          $mods object $tname extends { ..$earlydefns } with ..$parents  { $self =>
            ..${body.map(b => Util.addAnnotation(c)(b, annoDiffable, _ => true))}

            ..${if(hasCollectionParam) Seq(convertFunction) else Seq()}
          }
         """
    }

    def rewriteParam(param: Tree): Tree = param match {
      case q"$mods val $p: $tp = $rhs" =>
        q"$mods val $p: ${rewriteParamType(tp)} = $rhs"
    }
    def rewriteParamType(tp: Tree): Tree = tp match {
      case tq"$_[$targ]" =>
        val ty = Util.treeType(c)(tp)
        if (ty <:< typeOf[Option[_]]) {
          hasCollectionParam = true
          tq"$tDiffableOption[${rewriteParamType(targ)}]"
        } else if (ty <:< typeOf[Seq[_]]) {
          hasCollectionParam = true
          tq"$tDiffableList[${rewriteParamType(targ)}]"
        } else
          tp
      case _ => tp
    }
    def asType(tp: Tree): Tree = tp match {
      case tq"$_[$targ]" =>
        val ty = Util.treeType(c)(tp)
        if (ty <:< typeOf[Option[_]]) {
          q"$oOptionType(${asType(targ)})"
        } else if (ty <:< typeOf[DiffableOption[_]]) {
          q"$oOptionType(${asType(targ)})"
        } else if (ty <:< typeOf[Seq[_]]) {
          q"$oListType(${asType(targ)})"
        } else if (ty <:< typeOf[DiffableList[_]]) {
          q"$oListType(${asType(targ)})"
        } else
          q"$oSortType(classOf[$tp])"
      case _ => q"$oSortType(classOf[$tp])"
    }
    def paramConverter(tp: Tree, arg: Tree): Tree = tp match {
      case tq"$_[$targ]" =>
        val ty = Util.treeType(c)(tp)
        if (ty <:< tyDiffable)
          arg
        else if (ty <:< typeOf[Option[_]])
          q"$oDiffableOption.from(${paramConverterRec(targ, arg)}, ${asType(targ)})"
        else if (ty <:< typeOf[Seq[_]])
          q"$oDiffableList.from(${paramConverterRec(targ, arg)}, ${asType(targ)})"
        else
          arg
      case _ => arg
    }
    def paramConverterRec(tp: Tree, arg: Tree): Tree = tp match {
      case tq"$_[$targ]" =>
        val ty = Util.treeType(c)(tp)
        if (ty <:< typeOf[Option[_]])
          q"$arg.map(a => $oDiffableOption.from(${paramConverterRec(targ, q"a")}, ${asType(targ)}))"
        else if (ty <:< typeOf[Seq[_]])
          q"$arg.map(xs => $oDiffableList.from(${paramConverterRec(targ, q"xs")}, ${asType(targ)}))"
        else
          arg
      case _ => arg
    }
    def convertFunction: DefDef =
      q"""
          def apply(..${classParamss}): $classTp =
            $oThis(..${classParamss.map{case q"$mods val $p: $tp = $rhs" => paramConverter(tp, q"$p")}})
        """

    val mappedAnnottees = annottees.map(a => rewrite(a))

    if (annottees.size > 1 || !hasCollectionParam) {
      val res = q"{..$mappedAnnottees}"
      println(res)
      res
    } else {
      val companion =
        q"""
            object $oThis {
              $convertFunction
            }
          """
      val extendedRes = q"{${mappedAnnottees.head}; $companion}"
      println(extendedRes)
      extendedRes
    }

  }
}