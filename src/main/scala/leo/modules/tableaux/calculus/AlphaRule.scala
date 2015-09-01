package leo.modules.tableaux.calculus

import leo.datastructures._

/**
 *
 * Implementation of the Alpha Rule for Tableaux Calculus
 *
 * @since 9/1/15
 * @author Max Wisniewski
 */
object AlphaRule {
  def apply(t : Term) : Option[Seq[Term]] = t match {
    case &(t1, t2) => Some(List(t1,t2))
    case Not(|||(t1, t2)) => Some(List(Not(t1),Not(t2)))
    case Not(Not(t1)) => ???
    case Not(LitTrue) => Some(List(LitFalse))
    case Not(LitFalse) => Some(List(LitTrue))
  }
}
