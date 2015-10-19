package leo.modules.tableaux.agents


import leo.datastructures.blackboard.{DataType, DataEvent, Result, Event}
import leo.modules.tableaux.{TableauxFormula, TableauxFormulaType}
import leo.modules.tableaux.calculus.{DeltaRule}
import leo.modules.tableaux.datastructures.TableauxState
import leo.agents.{Task, Agent}

import scala.collection.concurrent.TrieMap


/**
 *
 * Agent to Perform the [[DeltaRule]] and insert the result into the [[TableauxState]].
 *
 * @author Max Wisniewski
 * @since 9/2/15.
 */
object DeltaRulaAgent extends Agent{
  override def name: String = "delta_rule"
  override val interest = Some(Seq(TableauxFormulaType))

  // Stores the amount of applications of the Delta rule, to allow the bid of new formulas to be higher.
  private[agents] val appAmount : TrieMap[TableauxFormula, Int] = new TrieMap[TableauxFormula, Int]()

  override def run(t: Task): Result = t match {
    case DeltaRuleTask(old, newF) =>
      appAmount.put(old, appAmount.getOrElse(old, 0)+1)
      Result().insert(TableauxFormulaType)(newF)
    case _ => Result()
  }

  override def toFilter(event: Event): Iterable[Task] = event match {
    case DataEvent(f : TableauxFormula, TableauxFormulaType) =>
      if(TableauxState.branchClosed(f.nodeid)) return Seq() // If the branch is already closed do not further track
      DeltaRule(f.term).fold(Seq() : Seq[Task]){t => Seq(DeltaRuleTask(f, TableauxFormula(t, f.nodeid)))}
    case _ => Seq()
  }
}

object DeltaRuleTask {
  def apply(f : TableauxFormula, newf : TableauxFormula) : DeltaRuleTask = new DeltaRuleTask(f, newf)
  def unapply(t : Task) : Option[(TableauxFormula, TableauxFormula)] = t match {
    case f : DeltaRuleTask => Some((f.oFormula, f.newFormula))
    case _ => None
  }
}

class DeltaRuleTask(val oFormula : TableauxFormula, val newFormula : TableauxFormula) extends Task {
  override def name: String = "delta_rule"
  override def writeSet(): Map[DataType, Set[Any]] = Map()
  override def readSet(): Map[DataType, Set[Any]] = Map(TableauxFormulaType -> Set(oFormula))
  override def bid(budget: Double): Double = budget / (oFormula.term.size + DeltaRulaAgent.appAmount.getOrElse(oFormula, 0))
  override def pretty: String = s"delta_split(${oFormula.term.pretty}) to [${newFormula.term.pretty}]"
}