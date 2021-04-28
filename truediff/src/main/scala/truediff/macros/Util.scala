package truediff.macros

import scala.reflect.macros.{TypecheckException, whitebox}

object Util {
  def treeType(c: whitebox.Context)(tp: c.Tree): c.Type = {
    import c.universe._
    val t = q"{type $$TypeTest = ${tp.asInstanceOf[c.Tree]}; ()}"
    try {
      val tt = c.typecheck(t)
      val q"{type $$TypeTest = $ttp; ()}" = tt
      ttp.tpe
    } catch {
      case _: TypecheckException => typeOf[Any]
    }
  }

  def mapParams[A](c: whitebox.Context)(
    paramss: Seq[Seq[c.Tree]],
    splitType: c.Type,
    sub: c.TermName => A,
    notSub: c.TermName => A,
  ): Seq[A] = {
    import c.universe._
    for (ps <- paramss;
         q"$_ val $p: $tp = $_" <- ps;
         ty = Util.treeType(c)(tp))
      yield
        if (ty <:< splitType)
          sub(p)
        else
          notSub(p)
  }

  def mapParamsTyped[A](c: whitebox.Context)(
    paramss: Seq[Seq[c.Tree]],
    splitType: c.Type,
    sub: (c.TermName, c.Tree) => A,
    notSub: (c.TermName, c.Tree) => A,
  ): Seq[A] = {
    import c.universe._
    for (ps <- paramss;
         q"$_ val $p: $tp = $_" <- ps;
         ty = Util.treeType(c)(tp))
      yield
        if (ty <:< splitType)
          sub(p, tp)
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

  def isProperSubtypeOf(c: whitebox.Context)(tp: c.Tree, ty: c.Type): Boolean = {
    import c.universe._
    tp match {
      case q"${tq"$name[..$targs]"}(...$_)" =>
        val tpTy = treeType(c)(tq"$name[..$targs]")
        tpTy <:< ty && !(tpTy =:= ty)
      case tq"$name[..$targs]" =>
        val tpTy = treeType(c)(tq"$name[..$targs]")
        tpTy <:< ty && !(tpTy =:= ty)
      case _ => false
    }
  }

  def isSupertypeOf(c: whitebox.Context)(tp: c.Tree, ty: c.Type): Boolean = {
    import c.universe._
    tp match {
      case q"${tq"$name[..$targs]"}(...$_)" => ty <:< treeType(c)(tq"$name[..$targs]")
      case tq"$name[..$targs]" => ty <:< treeType(c)(tq"$name[..$targs]")
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

  def boxedClassOf(c: whitebox.Context)(t: c.Tree): c.Tree = {
    import c.universe._
    val ty = treeType(c)(t)
    if (ty <:< typeOf[Boolean])
      q"classOf[java.lang.Boolean]"
    else if (ty <:< typeOf[Byte])
      q"classOf[java.lang.Byte]"
    else if (ty <:< typeOf[Short])
      q"classOf[java.lang.Short]"
    else if (ty <:< typeOf[Int])
      q"classOf[java.lang.Integer]"
    else if (ty <:< typeOf[Long])
      q"classOf[java.lang.Long]"
    else if (ty <:< typeOf[Float])
      q"classOf[java.lang.Float]"
    else if (ty <:< typeOf[Double])
      q"classOf[java.lang.Double]"
    else if (ty <:< typeOf[Char])
      q"classOf[java.lang.Character]"
    else if (ty <:< typeOf[String])
      q"classOf[java.lang.String]"
    else
      q"classOf[$t]"
  }

}
