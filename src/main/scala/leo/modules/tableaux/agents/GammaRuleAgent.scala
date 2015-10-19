package leo.modules.tableaux.agents

import leo.datastructures.blackboard.{DataType, DataEvent, Result, Event}
import leo.modules.tableaux.{TableauxFormula, TableauxFormulaType}
import leo.modules.tableaux.calculus.{GammaRule}
import leo.modules.tableaux.datastructures.TableauxState
import leo.agents.{Task, Agent}


/**
 *
 * Agent to Perform the [[GammaRule]] and insert the result into the [[TableauxState]].
 *
 * @author Max Wisniewski
 * @since 9/2/15.
 */
object GammaRuleAgent extends Agent{
  override def name: String = "gamma_rule"
  override val interest = Some(Seq(TableauxFormulaType))

  override def run(t: Task): Result = t match {
    case GammaRuleTask(old, newF) =>
      Result().insert(TableauxFormulaType)(newF)
    case _ => Result()
  }

  override def toFilter(event: Event): Iterable[Task] = event match {
    case DataEvent(f : TableauxFormula, TableauxFormulaType) =>
      if(TableauxState.branchClosed(f.nodeid)) return Seq() // If the branch is already closed do not further track
      GammaRule(f.term).fold(Seq() : Seq[Task]){t => Seq(GammaRuleTask(f, TableauxFormula(t, f.nodeid)))}
    case _ => Seq()
  }
}

object GammaRuleTask {
  def apply(f : TableauxFormula, newf : TableauxFormula) : GammaRuleTask = new GammaRuleTask(f, newf)
  def unapply(t : Task) : Option[(TableauxFormula, TableauxFormula)] = t match {
    case f : GammaRuleTask => Some((f.oFormula, f.newFormula))
    case _ => None
  }
}

class GammaRuleTask(val oFormula : TableauxFormula, val newFormula : TableauxFormula) extends Task {
  override def name: String = "gamma_rule"
  override def writeSet(): Map[DataType, Set[Any]] = Map()
  override def readSet(): Map[DataType, Set[Any]] = Map(TableauxFormulaType -> Set(oFormula))
  override def bid(budget: Double): Double = budget / oFormula.term.size
  override def pretty: String = s"gamma_split(${oFormula.term.pretty}) to [${newFormula.term.pretty}]"
}