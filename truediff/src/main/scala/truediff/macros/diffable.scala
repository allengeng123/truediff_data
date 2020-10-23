package truediff.macros

import truechange._
import truediff._

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
    val tNodeTag = symbolOf[Tag]
    val tHashable = symbolOf[Hashable]
    val oHashable = tHashable.companion
    val tyHashable = typeOf[Hashable]
    val tUnit = symbolOf[Unit]
    val tInt = symbolOf[Int]
    val tArray = symbolOf[Array[_]]
    val oArray = tArray.companion
    val tByte = symbolOf[Byte]
    val oBigInt = symbolOf[BigInt.type].asClass.module
    val oSeq = symbolOf[Seq.type].asClass.module
    val oMath = symbolOf[Math].companion
    val tURI = symbolOf[URI]
    val tSubtreeRegistry = symbolOf[SubtreeRegistry]
    val tDiffableOption = symbolOf[DiffableOption[_]]
    val oDiffableOption = tDiffableOption.companion
    val tDiffableList = symbolOf[DiffableList[_]]
    val oDiffableList = tDiffableList.companion
    val tIterable = symbolOf[Iterable[_]]
    val oIterable = symbolOf[Iterable.type].asClass.module
    val oJavaLitType = symbolOf[JavaLitType].companion

    val tLink = symbolOf[Link]
    val oNamedLink = symbolOf[NamedLink].companion
    val oLoadNode = symbolOf[Load].companion
    val oUnloadNode = symbolOf[Unload].companion
    val oUpdateLiteralsNode = symbolOf[UpdateLiterals].companion

    val tEditScriptBuffer = symbolOf[EditScriptBuffer]

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
        val thisType = tq"$tpname[..${tparams.map(_ => WildcardType)}]"

        oThis = TermName(tpname.toString)

        val wat = (p: TermName) => throw new UnsupportedOperationException(s"parameter $p of $tpname")
        val watt = (p: TermName,_:Tree) => throw new UnsupportedOperationException(s"parameter $p of $tpname")

        def mapAllParams[A](diffable: TermName => A, nonDiffable: TermName => A): Seq[A] =
          Util.mapParams(c)(paramss, tyDiffable, diffable, nonDiffable)
        def mapAllParamsTyped[A](diffable: (TermName,Tree) => A, nonDiffable: (TermName,Tree) => A): Seq[A] =
          Util.mapParamsTyped(c)(paramss, tyDiffable, diffable, nonDiffable)

        def mapDiffableParams[A](diffable: TermName => A): Seq[A] =
          Util.mapParams(c)(paramss, tyDiffable, p => Some(diffable(p)), _ => None).flatten
        def mapDiffableParamsTyped[A](diffable: (TermName,Tree) => A): Seq[A] =
          Util.mapParamsTyped(c)(paramss, tyDiffable, (p,t) => Some(diffable(p,t)), (_,_) => None).flatten

        def mapNonDiffableParams[A](nonDiffable: TermName => A): Seq[A] =
          Util.mapParams(c)(paramss, tyDiffable, _ => None, p => Some(nonDiffable(p))).flatten
        def mapNonDiffableParamsTyped[A](nonDiffable: (TermName,Tree) => A): Seq[A] =
          Util.mapParamsTyped(c)(paramss, tyDiffable, (_,_) => None, (p,t) => Some(nonDiffable(p,t))).flatten

        def nondiffableCond(other: Tree) =
          Util.reduceInfix(c)(mapNonDiffableParams(p => q"this.$p == $other.$p"), TermName("$amp$amp"), q"")
        def nondiffableCondNegated(other: Tree) =
          Util.reduceInfix(c)(mapNonDiffableParams(p => q"this.$p != $other.$p"), TermName("$bar$bar"), q"")

        val diffableParams: Seq[TermName] = mapDiffableParams(p=>p)

        def link(p: TermName, tp: Tree) = q"$oNamedLink(${p.toString})"

        val superDiffable = parents.find(tp => Util.isSubtypeOf(c)(tp, tyDiffable))
        val sort = if (superDiffable.isDefined) asType(superDiffable.get) else q"$oSortType(classOf[$thisType].getCanonicalName)"
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
                this.getClass.getSimpleName + "_" + this.uri.toString + paramStrings.mkString("(", ",", ")")
              }

            override lazy val identityHash: $tArray[$tByte] = {
              val digest = $oHashable.mkDigest
              digest.update(this.getClass.getCanonicalName.getBytes)
              ..${Util.mapParams(c)(paramss, tyHashable,
                p => q"digest.update(this.$p.identityHash)",
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
                Map(..${mapNonDiffableParamsTyped((p,t) => q"(${p.toString}, $oJavaLitType(${Util.boxedClassOf(c)(t)}))")})
            )

            override protected def assignSharesRecurse(that: $tDiffable, subtreeReg: $tSubtreeRegistry): $tUnit = that match {
              case that: $thisType if ${nondiffableCond(q"that")} =>
                ..${mapDiffableParams(p => q"this.$p.assignShares(that.$p, subtreeReg)")}
              case _ =>
                this.foreachSubtree(subtreeReg.assignShareAndRegisterTree)
                that.foreachSubtree(subtreeReg.assignShare)
            }

            override protected def directSubtrees: $tIterable[$tDiffable] =
              ${diffableParams match {
                case Seq() =>
                  q"$oIterable.empty"
                case Seq(p1) =>
                  q"$oIterable.single(this.$p1)"
                case ps =>
                  q"$oIterable(..${ps.map(p => q"this.$p")})"
              }}

            override protected def computeEditScriptRecurse(that: $tDiffable, parent: $tURI, parentTag: $tNodeTag, link: $tLink, edits: $tEditScriptBuffer): $tDiffable = that match {
              case that: $thisType if ${nondiffableCond(q"that")} =>
                ..${mapAllParamsTyped(
                  (p,t) => q"val $p = this.$p.computeEditScript(that.$p, this.uri, this.tag, ${link(p, t)}, edits).asInstanceOf[$t]",
                  (p,t) => q"val $p = this.$p"
                )}
                val $$newtree = $oThis(..${mapAllParams(p => q"$p", p => q"$p")})
                $$newtree._uri = this.uri
                $$newtree
              case _ => null
            }

            override def updateLiterals(that: $tDiffable, edits: $tEditScriptBuffer): $tDiffable = that match {
             case that: $thisType =>
                if (${nondiffableCondNegated(q"that")}) {
                  edits += $oUpdateLiteralsNode(this.uri, this.tag,
                    $oSeq(..${mapNonDiffableParams(p => q"(${p.toString}, this.$p)")}),
                    $oSeq(..${mapNonDiffableParams(p => q"(${p.toString}, that.$p)")})
                  )
                }

                ..${mapDiffableParamsTyped(
                  (p,t) => q"val $p = this.$p.updateLiterals(that.$p, edits).asInstanceOf[$t]"
                )}
                val newtree = $oThis(..${mapAllParams(p => q"$p", p => q"that.$p")})
                newtree._uri = this.uri
                newtree
            }

            override def loadUnassigned(edits: $tEditScriptBuffer): $tDiffable = {
              val that = this
              if (that.assigned != null) {
                return that.assigned.updateLiterals(that, edits)
              }

              ..${mapAllParamsTyped(
                (p,t) => q"val $p = that.$p.loadUnassigned(edits).asInstanceOf[$t]",
                (p,t) => q"val $p = that.$p"
              )}
              val $$newtree = $oThis(..${mapAllParams(p => q"$p", p => q"$p")})
              edits += $oLoadNode($$newtree.uri, this.tag,
                $oSeq(..${mapDiffableParamsTyped((p,t) => q"(${p.toString}, $p.uri)")}),
                $oSeq(..${mapNonDiffableParams(p => q"(${p.toString}, $p)")}),
              )
              $$newtree
            }

            override def loadInitial(edits: $tEditScriptBuffer): $tUnit = {
              ..${mapDiffableParams(p => q"this.$p.loadInitial(edits)")}
              edits += $oLoadNode(this.uri, this.tag,
                $oSeq(..${mapDiffableParamsTyped((p,t) => q"(${p.toString}, this.$p.uri)")}),
                $oSeq(..${mapNonDiffableParams(p => q"(${p.toString}, this.$p)")})
              )
            }


            override def unloadUnassigned(edits: $tEditScriptBuffer): $tUnit = {
              if (this.assigned != null) {
                this.assigned = null
              } else {
                edits += $oUnloadNode(this.uri, this.tag,
                  $oSeq(..${mapDiffableParamsTyped((p,t) => q"(${p.toString}, this.$p.uri)")}),
                  $oSeq(..${mapNonDiffableParams(p => q"(${p.toString}, this.$p)")})
                )
                ..${mapDiffableParamsTyped(
                  (p,t) => q"this.$p.unloadUnassigned(edits)"
                )}
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
        val ptype = rewriteParamType(tp).getOrElse(tp)
        q"$mods val $p: $ptype = $rhs"
    }
    def rewriteParamType(tp: Tree): Option[Tree] = tp match {
      case tq"$_[$targ]" =>
        val ty = Util.treeType(c)(tp)
        if (ty <:< tyDiffable) {
          Some(tp)
        } else if (ty <:< typeOf[Option[_]]) {
          val arg = rewriteParamType(targ).getOrElse(return None)
          hasCollectionParam = true
          Some(tq"$tDiffableOption[$arg]")
        } else if (ty <:< typeOf[Seq[_]]) {
          val arg = rewriteParamType(targ).getOrElse(return None)
          hasCollectionParam = true
          Some(tq"$tDiffableList[$arg]")
        } else {
          None
        }
      case _ =>
        val ty = Util.treeType(c)(tp)
        if (ty <:< tyDiffable) {
          Some(tp)
        } else {
          None
        }
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
          q"$oSortType(classOf[$tp].getCanonicalName)"
      case _ => q"$oSortType(classOf[$tp].getCanonicalName)"
    }
    def paramConverter(tp: Tree, arg: Tree): Tree = tp match {
      case tq"$_[$targ]" =>
        val ty = Util.treeType(c)(tp)
        if (ty <:< typeOf[Option[_]]) {
          val param = paramConverterRec(targ, arg).getOrElse(return arg)
          q"$oDiffableOption.from($param, ${asType(targ)})"
        }
        else if (ty <:< typeOf[Seq[_]]) {
          val param = paramConverterRec(targ, arg).getOrElse(return arg)
          q"$oDiffableList.from($param, ${asType(targ)})"
        } else
          arg
      case _ => arg
    }
    def paramConverterRec(tp: Tree, arg: Tree): Option[Tree] = tp match {
      case tq"$_[$targ]" =>
        val ty = Util.treeType(c)(tp)
        if (ty <:< tyDiffable) {
          Some(arg)
        } else if (ty <:< typeOf[Option[_]]) {
          val sub = paramConverterRec(targ, q"a").getOrElse(return None)
          Some(q"$arg.map(a => $oDiffableOption.from($sub, ${asType(targ)}))")
        } else if (ty <:< typeOf[Seq[_]]) {
          val sub = paramConverterRec(targ, q"xs").getOrElse(return None)
          Some(q"$arg.map(xs => $oDiffableList.from($sub, ${asType(targ)}))")
        } else
          None
      case _ =>
        val ty = Util.treeType(c)(tp)
        if (ty <:< tyDiffable) {
          Some(arg)
        }
        else {
          None
        }
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