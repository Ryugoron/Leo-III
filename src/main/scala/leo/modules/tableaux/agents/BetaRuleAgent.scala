package leo
package modules.tableaux
package agents

import leo.datastructures.Term
import leo.datastructures.blackboard.{DataType, DataEvent, Result, Event}
import leo.modules.tableaux.calculus.BetaRule
import leo.modules.tableaux.datastructures.{TableauxTreeType, NodeIdentifier, TableauxState}
import leo.agents.{Task, Agent}

import scala.collection.concurrent.TrieMap

/**
 *
 * Agent to Perform the [[BetaRule]] and insert the result into the [[TableauxState]].
 *
 * @author Max Wisniewski
 * @since 9/2/15.
 */
object BetaRuleAgent extends Agent{
  override def name: String = "alpha_rule"
  override val interest = Seq(TableauxFormulaType, TableauxTreeType)

  //TODO Either this is a good option to deal with the problem, or find a generic solution
  // Stores the split causes, hence we can collect which alpha splits already happend
  private val excutedAlpha : TrieMap[Int, TableauxFormula] = new TrieMap[Int, TableauxFormula]()

  def executedBranch(nodeId : Int) : Set[TableauxFormula] = {
    var curId = nodeId
    var set : Set[TableauxFormula] = Set()
    while(curId > 0){
      excutedAlpha.get(curId).map{f => set = set + f}
      curId = curId % 2
    }
    set
  }

  override def run(t: Task): Result = t match {
    case BetaRuleTask(f, fs, node) =>
      var r = Result()
      if(fs.size != 2 || node.leaf) return r.update(TableauxTreeType)(node)(node)   // Necessary, beacuse we took the write-lock TODO fix on Processing level
      val (newNode, left, right) = node.split
      val leftSplit = fs(0)
      val rightSplit = fs(1)
      r.insert(TableauxFormulaType)(TableauxFormula(leftSplit, left.id))
          .insert(TableauxFormulaType)(TableauxFormula(rightSplit, right.id))
          .insert(TableauxTreeType)(left)
          .insert(TableauxTreeType)(right)
          .update(TableauxTreeType)(node)(newNode)
    case _ => Result()
  }

  override def toFilter(event: Event): Iterable[Task] = event match {
    case DataEvent(f : TableauxFormula, TableauxFormulaType) =>
      if(TableauxState.branchClosed(f.nodeid)) return Seq() // If the branch is already closed do not further track

      // Get all Leaves of the branch `f` was inserted to and perform the split there,
      BetaRule(f.term).fold(Seq() : Seq[Task]) { t =>
        (TableauxState.getLeaveIdentifier(f.nodeid)).foldLeft(Seq(): Seq[Task]) { (ts, node) =>
          BetaRuleTask(f, t, node) +: ts
        }
      }
    case DataEvent(n : NodeIdentifier, TableauxTreeType) =>
      // If a new node is introduced reintroduce every
      if(!n.leaf) return Seq()
      // Collect all open alpha splits of the branch and insert them anew
      (TableauxState.getBranch(n) -- executedBranch(n.id)).foldLeft(Seq() : Seq[Task]){(ts, f) =>
        BetaRule(f.term).fold(ts){nts => BetaRuleTask(f, nts, n) +: ts}
      }
    case _ => Seq()
  }
}

object BetaRuleTask {
  def apply(f : TableauxFormula, fs : Seq[Term], nodeIdentifier: NodeIdentifier) : Task = new BetaRuleTask(f, fs, nodeIdentifier)
  def unapply(t : Task) : Option[(TableauxFormula, Seq[Term], NodeIdentifier)] = t match {
    case f : BetaRuleTask => Some((f.oFormula, f.newFormula, f.nodeIdentifier))
    case _ => None
  }
}

class BetaRuleTask(val oFormula : TableauxFormula, val newFormula : Seq[Term], val nodeIdentifier: NodeIdentifier) extends Task {
  override def name: String = "alpha_rule"
  override def writeSet(): Map[DataType, Set[Any]] = Map(TableauxTreeType -> Set(nodeIdentifier))
  override def readSet(): Map[DataType, Set[Any]] = Map(TableauxFormulaType -> Set(oFormula))
  override def bid(budget: Double): Double = budget / (oFormula.term.size)    // Add the amount of Quantifiers to the lower part
  override def pretty: String = s"alpha_split(${oFormula.term.pretty}) to [${newFormula.map(_.pretty).mkString(", ")}}]"
}
