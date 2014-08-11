package leo.datastructures.internal.terms

import leo.datastructures.Pretty


// TODO: Normalisation on subst needed?
/**
 * // TODO Doc
 */
sealed abstract class Subst extends Pretty {
  /** s.comp(s') = t
    * where t = s o s' */
  def comp(other: Subst): Subst
  def o = comp(_)

  /** Sink substitution inside lambda abstraction, i.e. create 1.s o ↑*/
  def sink: Subst = Cons(BoundFront(1),this o Shift(1))

  def normalize: Subst
}

/////////////////////////////////////////////////
// Implementation of substitutions
/////////////////////////////////////////////////

case class Shift(n: Int) extends Subst {
  def comp(other: Subst) = n match {
    case 0 => other
    case _ => other match {
      case Shift(0) => this
      case Shift(m) => Shift(n+m)
      case Cons(ft, s) => Shift(n-1).comp(s)
    }
  }

  val normalize = this

  /** Pretty */
  override def pretty = n match {
    case 0 => "id"
    case k => s"↑$k"
  }
}
case class Cons(ft: Front, subst: Subst) extends Subst {
  def comp(other: Subst) = other match {
    case Shift(0) => this
    case s => Cons(ft.substitute(s), subst.comp(s))
  }

  lazy val normalize = ft match {
    case BoundFront(_) => Cons(ft, subst.normalize)
    case TermFront(t)  => Cons(TermFront(t.normalize(Subst.id)), subst.normalize) //TODO: eta contract here
  }

  /** Pretty */
  override def pretty = s"${ft.pretty}•${subst.pretty}"
}


object Subst {
  def id: Subst    = Shift(0)
  def shift: Subst = shift(1)
  def shift(n: Int): Subst = Shift(n)
}


sealed abstract class Front extends Pretty {
  def substitute(subst: Subst): Front
}
case class BoundFront(n: Int) extends Front {
  def substitute(subst: Subst) = subst match {
    case Cons(ft, s) if n == 1 => ft
    case Cons(_, s)           => BoundFront(n-1).substitute(s)
    case Shift(k) => BoundFront(n+k)
  }

  /** Pretty */
  override def pretty = s"$n"
}
case class TermFront(term: Term) extends Front {
  def substitute(subst: Subst) = TermFront(term.closure((subst)))

  /** Pretty */
  override def pretty = term.pretty
}

/**
case object UNBOUND extends Front {
  def substitute(subst: Subst) = UNBOUND
}
  */
