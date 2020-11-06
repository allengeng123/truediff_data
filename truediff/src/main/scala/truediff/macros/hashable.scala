package truediff.macros


import truediff.Hashable

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.language.experimental.macros
import scala.reflect.macros.whitebox

@compileTimeOnly("Scala 2.13 and compiler flag -Ymacro-annotations required")
class hashable extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro HashableMacro.impl
}

object HashableMacro {
  def impl(c: whitebox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._

    val tHashable = symbolOf[Hashable]
    val oHashable = tHashable.companion
    val tyHashable = typeOf[Hashable]
    val tArray = symbolOf[Array[_]]
    val tByte = symbolOf[Byte]
    val oBigInt = symbolOf[BigInt.type].asClass.module
    val annoHashable = q"new _root_.truediff.macros.hashable()"

    def rewrite(t: Tree): Tree = t match {
      case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents with $tHashable { $self => ..$stats }"

      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        q"""
          $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents with $tHashable { $self =>
            ..$stats

            override lazy val literalsHash: $tArray[$tByte] = {
              val digest = $oHashable.mkDigest
              ..${Util.mapParams(c)(paramss, tyHashable,
                p => q"digest.update(this.$p.literalsHash)",
                p => q"$oHashable.hash(this.$p, digest)"
              )}
              digest.digest()
            }
          }
         """

      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
        q"""
          $mods object $tname extends { ..$earlydefns } with ..$parents with $tHashable  { $self =>
            ..${body.map(b => Util.addAnnotation(c)(b, annoHashable, _ => true))}
          }
         """
    }

    val res = q"{..${annottees.map(a => rewrite(a))}}"

//    println(res)
    res
  }
}