package leo
package agents
package impl

import leo.datastructures.blackboard._
import Store._
import leo.modules.proofCalculi.{BoolExt, BoolExtAlt}

/**
 * Created by lex on 11.05.15.
 */
object BoolExtAgent extends Agent {
  /**
   *
   * @return the name of the agent
   */
  def name = "Boolean Extensionality Agent"
  override val interest : Option[Seq[DataType]] = Some(List(FormulaType))
  /**
   * This function runs the specific agent on the registered Blackboard.
   */
  def run(t: Task): Result = {
    t match {
      case BoolExtTask(f, hint) => {
        val ncs = BoolExt.apply(f.clause, hint)
        Out.trace(s"[$name:]\n  Equalities in clause ${f.clause.pretty} replaced by equivalences\n New clauses: ${ncs.pretty}")
//        val r = Result()
//        ncs.foreach {nc => r.insert(FormulaType)(Store(nc, f.status, f.context))}
        Result().insert(FormulaType)(Store(ncs, f.status, f.context).newOrigin(List(f), BoolExt.name))
//        r
      }
      case _: Task =>
        Out.warn(s"[$name]: Got a wrong task to execute.");
        Result()
    }

  }

  /**
   * Triggers the filtering of the Agent.
   *
   * Upon an Event the Agent can generate Tasks, he wants to execute.
   * @param event on the blackboard concerning change of data.
   * @return a List of Tasks the Agent wants to execute.
   */
  def toFilter(event: Event) = {
    event match {
      case DataEvent(f: FormulaStore, FormulaType) => {
        val (canApply, hint) = BoolExt.canApply(f.clause)
        if (canApply) {
          Out.trace(s"[$name:]\n  Equalities in clause ${f.clause.pretty} can be replaced by equivalences")
          Seq(BoolExtTask(f, hint))
        } else {
          Seq()
        }
      }
      case _ : Event => Nil
    }
  }
}

private case class BoolExtTask(f: FormulaStore, hint: BoolExt.HintType) extends Task {
  def name = "bool_ext"
  def writeSet() = Set.empty
  def readSet() = Set(f)

  def bid(budget: Double) = budget*hint._1.size / (hint._1.size + hint._2.size)

  def pretty = s"bool_ext(${f.pretty})"
}