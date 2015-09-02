package leo
package modules.tableaux
package agents

import leo.datastructures.blackboard.{DataType, DataEvent, Result, Event}
import leo.modules.tableaux.calculus.{BetaRule}
import leo.modules.tableaux.datastructures.TableauxState
import leo.agents.{Task, Agent}


/**
 *
 * Agent to Perform the [[BetaRule]] and insert the result into the [[TableauxState]].
 *
 * @author Max Wisniewski
 * @since 9/2/15.
 */
object AlphaRuleAgent extends Agent{
  override def name: String = "beta_rule"
  override val interest = Seq(TableauxFormulaType)

  override def run(t: Task): Result = t match {
    case AlphaRuleTask(old, newFs) =>
      newFs.foldLeft(Result()){(r,f) => r.insert(TableauxFormulaType)(f)}
    case _ => Result()
  }

  override def toFilter(event: Event): Iterable[Task] = event match {
    case DataEvent(f : TableauxFormula, TableauxFormulaType) =>
      if(TableauxState.branchClosed(f.nodeid)) return Seq() // If the branch is already closed do not further track
      BetaRule(f.term).fold(Seq() : Seq[Task]){ts => Seq(AlphaRuleTask(f, ts.map{t => TableauxFormula(t, f.nodeid)}))}
    case _ => Seq()
  }
}

object AlphaRuleTask {
  def apply(f : TableauxFormula, fs : Seq[TableauxFormula]) : AlphaRuleTask = new AlphaRuleTask(f, fs)
  def unapply(t : Task) : Option[(TableauxFormula, Seq[TableauxFormula])] = t match {
    case f : AlphaRuleTask => Some((f.oFormula, f.newFormula))
    case _ => None
  }
}

class AlphaRuleTask(val oFormula : TableauxFormula, val newFormula : Seq[TableauxFormula]) extends Task {
  override def name: String = "beta_rule"
  override def writeSet(): Map[DataType, Set[Any]] = Map()
  override def readSet(): Map[DataType, Set[Any]] = Map(TableauxFormulaType -> Set(oFormula))
  override def bid(budget: Double): Double = budget / oFormula.term.size
  override def pretty: String = s"beta_split(${oFormula.term.pretty}) to [${newFormula.map(_.term.pretty).mkString(", ")}}]"
}
