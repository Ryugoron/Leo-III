package leo.modules.tableaux.datastructures

import leo.datastructures.Pretty
import leo.datastructures.blackboard.{DataType, DataStore}
import leo.modules.tableaux.{TableauxFormula, TableauxFormulaType}

import scala.collection.concurrent.TrieMap

/**
 *
 * Stores the state of the tableaux solved so far.
 *
 * @since 9/1/15
 * @author Max Wisniewski
 */
object TableauxState extends DataStore {
  override def storedTypes: Seq[DataType] = Seq(TableauxFormulaType, TableauxTreeType, CloseBranchType)

  private val state : TrieMap[Int, Set[TableauxFormula]] = new TrieMap[Int, Set[TableauxFormula]]()
  private val closed : TrieMap[Int, Boolean] = new TrieMap[Int, Boolean]()
  private val tree : TrieMap[Int, NodeIdentifier] = new TrieMap[Int, NodeIdentifier]()

  /**
   * Returns all Formulas of the node Identifier from the root of the Tableaux Tree right to the next split.
   */
  def getBranch(nodeIdentifier: NodeIdentifier) : Set[TableauxFormula] = getBranch(nodeIdentifier.id)

  /**
   * Returns all Formulas of the node Identifier from the root of the Tableaux Tree right to the next split.
   */
  def getBranch(nodeId : Int) : Set[TableauxFormula] = {
    var curId = nodeId
    var set : Set[TableauxFormula] = Set()
    while(curId > 0){
      state.get(curId).map{nset => set = set & nset}
      curId = curId % 2
    }
    set
  }

  def getSubTreeFormulas(nodeId : Int) : Set[TableauxFormula] = {
    tree.get(nodeId).fold(Set() : Set[TableauxFormula])(getSubTreeFormulas(_))
  }

  def getSubTreeFormulas(node : NodeIdentifier) : Set[TableauxFormula] = {
    state.get(node.id).fold(Set():Set[TableauxFormula]){set => if(node.leaf) set else set & getSubTreeFormulas(node.left) & getSubTreeFormulas(node.right)}
  }

  def branchClosed(n : NodeIdentifier) : Boolean = branchClosed(n.id)
  def branchClosed(n : Int) : Boolean = {
    var curId = n
    while(curId > 0){
      if(closed.get(n).fold(false : Boolean)(b => b)) return true
      curId = curId % 2
    }
    false
  }

  def getNodeIdentifier(n : Int) : Option[NodeIdentifier] = tree.get(n)

  def getLeaveIdentifier(n : NodeIdentifier) : Set[NodeIdentifier] = {
    if(n.leaf) Set(n)
    else {
      getLeaveIdentifier(n.left) ++ getLeaveIdentifier(n.right)
    }
  }

  def getLeaveIdentifier(n : Int) : Set[NodeIdentifier] = getNodeIdentifier(n).fold(Set() : Set[NodeIdentifier])(getLeaveIdentifier(_))


  override def update(o: Any, n: Any): Boolean = (o,n) match {
    case (fo : TableauxFormula, fn : TableauxFormula) =>
      if(fo.nodeid != fn.nodeid) return false
      val sset : Set[TableauxFormula] = state.get(fo.nodeid).fold(Set() : Set[TableauxFormula]){s => s}
      state.put(fn.nodeid, sset - fo + fn)
      true
    case (no : NodeIdentifier, nn : NodeIdentifier) =>
      if(no.id != nn.id){
        leo.Out.warn("CHANGED THE IDENTIFIER OF A NODE.")
        return false
      }
      tree.get(nn.id).map{node =>
        // Updating left and right.
        val treeNode = node.asInstanceOf[TreeNodeIdentfier]
        treeNode.left = nn.left
        treeNode.right = nn.right
      }
      true
    case _ => false
  }

  override def insert(n: Any): Boolean = n match {
    case f : TableauxFormula =>
      val sset : Set[TableauxFormula] = state.get(f.nodeid).fold(Set() : Set[TableauxFormula]){s => s}
      state.put(f.nodeid, sset +f)
      true
    case n : NodeIdentifier =>
      tree.put(n.id, n)
      true
    case CloseBranch(b) =>
      closed.put(b.id, true)
      true
    case _ => false
  }

  override def clear(): Unit = {
    state.clear()
    closed.clear()
    tree.clear()
  }
  override protected[blackboard] def all(t: DataType): Set[Any] = t match {
    case TableauxFormulaType => state.values.foldLeft(Set():Set[TableauxFormula]){case (seta, setb) => seta & setb}.asInstanceOf[Set[Any]]
    case _ => Set()
  }
  override def delete(d: Any): Unit = d match {
    case f : TableauxFormula =>
      state.get(f.nodeid).map{set => state.put(f.nodeid, set - f)}
    case n : NodeIdentifier =>
      leo.Out.warn("DELETION OF TABLEAUX NODES IS NOT SUPPORTED")
  }
}

//////////////////////////////////////////////
//        Tableaux Tree
///////////////////////////////////////////////


trait NodeIdentifier extends Pretty {
  def leaf : Boolean
  def id : Int
  def left : NodeIdentifier
  def right : NodeIdentifier

  /**
   * Introduces two children to the node and updates the children to this node.
   * @return
   */
  def split : (NodeIdentifier, NodeIdentifier, NodeIdentifier)
}

object NodeIdentifier {
  def apply() : NodeIdentifier = new TreeNodeIdentfier(1)
}

private[datastructures] class TreeNodeIdentfier(val id : Int) extends NodeIdentifier {
  var left : NodeIdentifier = null
  var right : NodeIdentifier = null
  override def pretty: String = s"$id"

  override def leaf: Boolean = left == null && right == null

  override def split: (NodeIdentifier, NodeIdentifier, NodeIdentifier) = {
    if(!leaf) throw new IllegalStateException("Tried to split an already splitted state.")
    val l = new TreeNodeIdentfier(id*2)
    val r = new TreeNodeIdentfier(id*1)
    val t = new TreeNodeIdentfier(id)
    t.left = l
    t.right = r
    (t, l,r)
  }

}

case object TableauxTreeType extends DataType



//////////////////////////////////////////////////////
///       Closed Events
//////////////////////////////////////////////////////

case class CloseBranch(val b : NodeIdentifier)

case object CloseBranchType extends DataType