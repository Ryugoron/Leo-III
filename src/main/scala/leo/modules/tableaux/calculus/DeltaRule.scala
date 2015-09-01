package leo.modules.tableaux.calculus

import leo.datastructures.{Exists, Forall, Term, Not}
import leo.datastructures.Term.:::>

/**
 *
 * Implementation of the Delta Rule for Tableaux Calculus
 *
 * @since 9/1/15
 * @author Max Wisniewski
 */
object DeltaRule {
  def apply(t : Term) : Option[Term] = t match{
    case Forall(ty :::> t) =>
      val m = Term.mkFreshMetaVar(ty)
      Some(Term.mkTermApp(Term.mkTermAbs(ty, t), m).betaNormalize)
    case Not(Exists(ty :::> t)) =>
      val m = Term.mkFreshMetaVar(ty)
      Some(Not(Term.mkTermApp(Term.mkTermAbs(ty, t), m).betaNormalize))
    case _ => None
  }
}
