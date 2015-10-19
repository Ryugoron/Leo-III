package leo.modules.tableaux.agents

import leo.agents.{Task, Agent}
import leo.datastructures.Term
import leo.datastructures.blackboard.{DataType, DataEvent, Result, Event}
import leo.modules.tableaux.datastructures._
import leo.modules.tableaux.unification.FOUnification
import leo.modules.tableaux.{TableauxFormulaType, TableauxFormula}

/**
 *
 * The closing agent has to search
 * upon insertion of a formula `p` into a branch for a formula `Not(q)`
 * and `p` and `q` are unifyiable. This closes:
 * <ol>
 *   <li> the branch of `p` if `q` occurs above `p`</li>
 *   <li> the branch of `q` if `q` occurs below `p`</li>
 * </ol>
 *
 * The resulting substitution is inserted into a substitution store, that saves all substitutions
 * performed during the proof search.<br /><br />
 * <b>Important to note:</b><br />
 * Both formulas `p` and `q` are only considered, if they contain no meta variables occuring in one
 * of the substitutions in the substition store. Since all meta variables are rigid quantified, there could not
 * be two different substitutions for one and the same meta variable.
 *
 * @since 9/2/15
 * @author Max Wisniewski
 */
object ClosingAgent extends Agent{
  /**
   *
   * @return the name of the agent
   */
  override def name: String = "branch_close"

  /**
   * This function runs the specific agent on the registered Blackboard.
   */
  override def run(t: Task): Result = t match {
    case ClosingTask(f1, f2, sub, nId) if (!SubstitionState.exsitsSubstitution(f1.term) && !SubstitionState.exsitsSubstitution(f2.term)) =>
      Result().insert(CloseBranchType)(new CloseBranch(nId))
        .insert(SubstitutionType)(sub)
    case _ => Result()
  }

  /**
   * Triggers the filtering of the Agent.
   *
   * Upon an Event the Agent can generate Tasks, he wants to execute.
   * @param event on the blackboard concerning change of data.
   * @return a List of Tasks the Agent wants to execute.
   */
  override def toFilter(event: Event): Iterable[Task] = event match {
    case DataEvent(f : TableauxFormula, TableauxFormulaType) =>
      // First check all upper formulas
      if(SubstitionState.exsitsSubstitution(f.term)) return Seq()
      val tasks : Seq[Task] = TableauxState.getBranch(f.nodeid).foldLeft(Seq() : Seq[Task]){case (l, f2) =>
        if(SubstitionState.exsitsSubstitution(f2.term))
          l
        else {
          val uni : Option[Map[Int, Term]] = FOUnification(f.term)(f2.term)
          uni.fold(l){u => new ClosingTask(f, f2, u, TableauxState.getNodeIdentifier(f.nodeid).get) +: l}
        }
      }
      TableauxState.getSubTreeFormulas(f.nodeid).foldLeft(tasks){case (l, f2) =>
        if(SubstitionState.exsitsSubstitution(f2.term))
          l
        else {
          val uni: Option[Map[Int, Term]] = FOUnification(f.term)(f2.term)
          uni.fold(l) { u => new ClosingTask(f, f2, u, TableauxState.getNodeIdentifier(f2.nodeid).get) +: l }
        }
      }
      tasks
  }
}


private case class ClosingTask(f1 : TableauxFormula, f2 : TableauxFormula, sub : Map[Int, Term], nodeIdentifier: NodeIdentifier) extends Task {
  override def name: String = "branch_close"
  override def writeSet(): Map[DataType, Set[Any]] = Map(CloseBranchType -> Set(nodeIdentifier))
  override def readSet(): Map[DataType, Set[Any]] = Map(TableauxFormulaType -> Set(f1,f2))
  override def bid(budget: Double): Double = budget / (nodeIdentifier.depth)    // Prefer to close nodes more up in the tree
  override def pretty: String = s"branch_close ${nodeIdentifier.pretty}"
}