package truediff.macros

import scala.reflect.macros.whitebox

object Util {
  def treeType(c: whitebox.Context)(tp: Any) = {
    import c.universe._
    val t = q"{type T = ${tp.asInstanceOf[c.Tree]}; ()}"
    val tt = c.typecheck(t)
    val q"{type T = $ttp; ()}" = tt
    ttp.tpe
  }

  def mapParams[A](c: whitebox.Context)(
    paramss: Seq[Seq[c.Tree]],
    splitType: c.Type,
    sub: c.TermName => A,
    notSub: c.TermName => A,
    option: c.TermName => A,
    seq: c.TermName => A
  ): Seq[A] = {
    import c.universe._
    for (ps <- paramss;
         q"$_ val $p: $tp = $_" <- ps;
         ty = Util.treeType(c)(tp))
      yield
        if (ty <:< splitType)
          sub(p)
        else if (ty <:< appliedType(typeOf[Option[_]].typeConstructor, splitType))
          option(p)
        else if (ty <:< appliedType(typeOf[Seq[_]].typeConstructor, splitType))
          seq(p)
        else
          notSub(p)
  }

  def mapParamsTyped[A](c: whitebox.Context)(
    paramss: Seq[Seq[c.Tree]],
    splitType: c.Type,
    sub: (c.TermName, c.Tree) => A,
    notSub: (c.TermName, c.Tree) => A,
    option: (c.TermName, c.Tree) => A,
    seq: (c.TermName, c.Tree) => A
  ): Seq[A] = {
    import c.universe._
    for (ps <- paramss;
         q"$_ val $p: $tp = $_" <- ps;
         ty = Util.treeType(c)(tp))
      yield
        if (ty <:< splitType)
          sub(p, tp)
        else if (ty <:< appliedType(typeOf[Option[_]].typeConstructor, splitType))
          option(p, tp)
        else if (ty <:< appliedType(typeOf[Seq[_]].typeConstructor, splitType))
          seq(p, tp)
        else
          notSub(p, tp)
  }

  def isSubtypeOf(c: whitebox.Context)(tp: c.Tree, ty: c.Type): Boolean = {
    import c.universe._
    tp match {
      case q"${tq"$name[..$targs]"}(...$_)" => treeType(c)(tq"$name[..$targs]") <:< ty
      case tq"$name[..$targs]" => treeType(c)(tq"$name[..$targs]") <:< ty
      case _ => false
    }
  }

  def addAnnotation(c: whitebox.Context)(t: c.Tree, anno: c.Tree, parentsCond: Seq[c.Tree] => Boolean): c.Tree = {
    import c.universe._
    t match {
      case q"$mods class $subname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        if (parentsCond(parents)) {
          val Modifiers(flags, privs, annos) = mods
          val newmods = Modifiers(flags, privs, annos :+ anno)
          q"$newmods class $subname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }"
        } else
          q"$mods class $subname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }"

      case q"$mods object $subname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
        if (parentsCond(parents)) {
          val Modifiers(flags, privs, annos) = mods
          val newmods = Modifiers(flags, privs, annos :+ anno)
          q"$newmods object $subname extends { ..$earlydefns } with ..$parents { $self => ..$body }"
        } else
          q"$mods object $subname extends { ..$earlydefns } with ..$parents { $self => ..$body }"

      case _ => t
    }
  }

  def nameOf(c: whitebox.Context)(t: c.Tree): c.Name = {
    import c.universe._
    t match {
      case q"$mods trait $tpname[..$tparams] extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        tpname

      case q"$mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" =>
        tpname

      case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$body }" =>
        tname
    }
  }

  def reduceInfix(c: whitebox.Context)(seq: Seq[c.Tree], op: c.TermName, nil: c.Tree): c.Tree = {
    import c.universe._
    if (seq.isEmpty)
      nil
    else
      seq.reduce((l, r) => q"$l $op $r")
  }

  def reducePrefix(c: whitebox.Context)(seq: Seq[c.Tree], op: c.Tree, nil: c.Tree): c.Tree = {
    import c.universe._
    if (seq.isEmpty)
      nil
    else
      seq.reduce((l, r) => q"$op($l, $r)")
  }

}
