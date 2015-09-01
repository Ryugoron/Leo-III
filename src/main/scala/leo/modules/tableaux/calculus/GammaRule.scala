package leo.modules.tableaux.calculus

import leo.datastructures.impl.Signature
import leo.datastructures.{Exists, Term, Not, Forall, Type}
import leo.datastructures.Term._

/**
 *
 * Implementation of the Gamma Rule for Tableaux Calculus
 *
 * @since 9/1/15
 * @author Max Wisniewski
 */
object GammaRule {
  def apply(t : Term) : Option[Term] = t match {
    case Exists(ty :::> t1) =>
      val m = constructSkolemVariable(ty)(t1.metaVars)
      Some(Term.mkTermApp(Term.mkTermAbs(ty, t), m).betaNormalize)
    case Not(Forall(ty :::> t1)) =>
      val m = constructSkolemVariable(ty)(t1.metaVars)
      Some(Not(Term.mkTermApp(Term.mkTermAbs(ty, t), m).betaNormalize))
    case _ => None
  }

  private def constructSkolemVariable(goalType : Type)(dep : Set[(Type, Int)]) : Term = {
    val deplist = dep.toList
    val ty = Type.mkFunType(deplist.map(_._1), goalType)
    val skoFun = Signature.get.freshSkolemVar(ty)
    mkTermApp(skoFun, deplist.map{case (ty, i) => Term.mkMetaVar(ty, i)})
  }
}
