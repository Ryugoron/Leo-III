package leo.modules.tableaux

import leo.datastructures.Term
import leo.datastructures.blackboard.DataType


object TableauxFormula {
  def unapply(x : TableauxFormula) : Option[(Term, Int)] = Some(x.term, x.nodeid)
  def apply(t : Term, nodeId : Int) : TableauxFormula = new TableauxFormula(t,nodeId)
}

/**
 *
 * Stores Formulas in the blackboard for the simple tableaux procedure.
 *
 * @since 9/1/15
 * @author Max Wisniewski
 */
class TableauxFormula (val term : Term, val nodeid : Int) {
  def map[A](f : Term => A) : A = f(term)
  def mapTerm(f : Term => Term) : TableauxFormula = new TableauxFormula(f(term), nodeid)
}

case object TableauxFormulaType extends DataType
