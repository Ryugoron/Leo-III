package leo.modules.tableaux.unification

import leo.datastructures.{===, Subst, Term, Type}
import leo.datastructures.Term._

/**
 *
 * Simple Method to compute the MGU for first-order terms.
 *
 * @since 9/1/15
 * @author Max Wisniewski
 */
object FOUnification {

  /**
   * Computes the most general unifier, if one exists.
   */
  def apply(t1 : Term)(t2 : Term) : Option[Subst] = (t1,t2) match {
    case (===(s1,t1), ===(s2,t2)) =>  // Handeling equality explicitly
      apply(s1)(s2).fold(None : Option[Subst]){sub => apply(t1.substitute(sub).betaNormalize)(t2.substitute(sub).betaNormalize).map(newsub => sub.comp(newsub))}
    case (MetaVar(m1), MetaVar(m2)) => //Both are meta variables, hence unifiable
        Some(Subst.id)
    case (MetaVar(t, i),t2) => // If the left hand side is a meta variable.
      if(t2.metaVars.contains((t,i)))
        None
      else
        Some(Subst.singleton(i, t2))
    case (t1, MetaVar(t, i)) => // If the left hand side is a meta variable.
      if(t1.metaVars.contains((t,i)))
        None
      else
        Some(Subst.singleton(i, t1))
    case (hd1 ∙ tail1, hd2 ∙ tail2) => // Both terms are not meta variables
      if(hd1 != hd2 || tail1.length != tail2.length) None
      else {
        tail1.zip(tail2).foldLeft(Some(Subst.id) : Option[Subst])(handleList)

      }
    case (ty1 :::> t1, ty2 :::> t2) => // Both are abstractions, in normal case this should never happen.
      apply(t1)(t2)
    case _ => None
  }

  private def handleList(osub : Option[Subst], ts: (Either[Term, Type], Either[Term, Type])) : Option[Subst] =
    ts match {
      case (Left(t1),Left(t2)) => osub.fold(None : Option[Subst]){sub => apply(t1.substitute(sub).betaNormalize)(t2.substitute(sub).betaNormalize).map{newsub => sub.comp(newsub)}}
      case _ => None
    }
}
