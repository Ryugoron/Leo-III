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
  def apply(t1 : Term)(t2 : Term) : Option[Map[Int, Term]] = (t1,t2) match {
    case (===(s1,t1), ===(s2,t2)) =>  // Handeling equality explicitly
      apply(s1)(s2).fold(None : Option[Map[Int,Term]]){sub => apply(t1.substitute(Subst.fromMap(sub)).betaNormalize)(t2.substitute(Subst.fromMap(sub)).betaNormalize).map(newsub => sub ++ newsub)}
    case (MetaVar(ty1,m1), MetaVar(ty2,m2)) => //Both are meta variables, hence unifiable
        if(m1 == m2) Some(Map()) else Some(Map(m1 -> t2))
    case (MetaVar(t, i),t2) => // If the left hand side is a meta variable.
      if(t2.metaVars.contains((t,i)))
        None
      else
        Some(Map(i -> t2))
    case (t1, MetaVar(t, i)) => // If the left hand side is a meta variable.
      if(t1.metaVars.contains((t,i)))
        None
      else
        Some(Map(i -> t1))
    case (hd1 ∙ tail1, hd2 ∙ tail2) => // Both terms are not meta variables
      if(hd1 != hd2 || tail1.length != tail2.length) None
      else {
        tail1.zip(tail2).foldLeft(Some(Map.empty) : Option[Map[Int, Term]])(handleList)

      }
    case (ty1 :::> t1, ty2 :::> t2) => // Both are abstractions, in normal case this should never happen.
      apply(t1)(t2)
    case _ => None
  }

  private def handleList(osub : Option[Map[Int, Term]], ts: (Either[Term, Type], Either[Term, Type])) : Option[Map[Int, Term]] =
    ts match {
      case (Left(t1),Left(t2)) => osub.fold(None : Option[Map[Int, Term]]){sub => apply(t1.substitute(Subst.fromMap(sub)).betaNormalize)(t2.substitute(Subst.fromMap(sub)).betaNormalize).map{newsub => sub ++ newsub}}
      case _ => None
    }
}
