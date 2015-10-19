package leo.modules.tableaux.agents

import leo.agents.{Task, Agent}
import leo.datastructures.blackboard.{DataType, DataEvent, Result, Event}
import leo.modules.tableaux.datastructures.{NodeIdentifier, TableauxState, CloseBranch, CloseBranchType}

/**
 *
 * An agent observing the tableaux state and if necessary closing the
 *
 * @author Max Wisniewski
 * @since 9/30/15
 */
object StateAgent extends Agent{
  /**
   *
   * @return the name of the agent
   */
  override def name: String = "closing_bubble"

  /**
   * This function runs the specific agent on the registered Blackboard.
   */
  override def run(t: Task): Result = ???

  /**
   * Triggers the filtering of the Agent.
   *
   * Upon an Event the Agent can generate Tasks, he wants to execute.
   * @param event on the blackboard concerning change of data.
   * @return a List of Tasks the Agent wants to execute.
   */
  override def toFilter(event: Event): Iterable[Task] = event match {
    case DataEvent(CloseBranchType, CloseBranch(id)) if id.id > 1 => // Otherwise this is the root of the tree
      TableauxState.getNodeIdentifier(id.id >> 1).fold(Seq() : Seq[Task]){parent =>
        if(TableauxState.branchClosed(parent.left) && TableauxState.branchClosed(parent.right))
          Seq(new CloseTask(parent))
        else
          Seq()
      }
    case _ => Seq()
  }
}

private case class CloseTask(n : NodeIdentifier) extends Task {
  override def name: String = "closing_bubble"
  override def writeSet(): Map[DataType, Set[Any]] = Map(CloseBranchType -> Set(n))
  override def readSet(): Map[DataType, Set[Any]] = Map()
  override def bid(budget: Double): Double = budget / n.depth
  override def pretty: String = s"closing_branch(${n.pretty})"
}
