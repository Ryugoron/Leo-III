package leo.modules.tableaux.datastructures

import leo.datastructures.impl.Signature
import leo.datastructures.{TermFront, BoundFront, Subst, Term}
import leo.datastructures.blackboard.{DataType, DataStore}

import scala.collection.concurrent.TrieMap

/**
 *
 * The substitution state accumulates all applied substitutions to
 * the Tableaux. It supports retrieval of single substitutions,
 * substitutions for one specific meta variable, and supplies a list
 * of all meta variables, that have to be substituted.
 *
 * @since 9/2/15
 * @author Max Wisniewski
 */
object SubstitionState extends DataStore {
  override def storedTypes: Seq[DataType] = Seq(SubstitutionType)

  // Interally ONE big substitution for all meta variables is saved
  private var sub : Subst = Subst.id                                          // Merging one time,
  private val metaMap : TrieMap[Int, Term] = new TrieMap[Int, Term]()         // but saving for each meta variable the substitution.


  /**
   * Represents the current substitution of all meta variables
   * in the tableaux.
   *
   * @return the current substitution
   */
  def getCurrentSubst : Subst = synchronized(sub)

  /**
   * Returns the term, that is currently substituted for a meta variable.
   *
   * @param metaIndex  the identifier for the meta variable
   * @return Some(Term) if (metaIndex -> Term) in the currentSubstitution else None
   */
  def metaToTerm(metaIndex : Int) : Option[Term] = metaMap.get(metaIndex)

  /**
   * Returns the single substitution for a meta variable in the current context.
   * Allows a one step substitution for only this meta variable
   *
   * @param metaIndex  the identifier for the meta variable
   * @return Subst.id, if the meta variable is not substituted in the currentSubstitution, otherwise
   *         (metaIndex -> metaToTerm(metaIndex))
   */
  def metaToSubst(metaIndex: Int) : Subst = Subst.fromMap(metaMap.toMap) //metaMap.get(metaIndex).fold(Subst.id)(Subst.singleton(metaIndex, _))


  /**
   * Checks a given term, if any substitution is available.
   *
   * @param t The term to check
   * @return true, iff there exists a substitution for t
   */
  def exsitsSubstitution(t : Term) : Boolean = {
    (metaMap.keySet & t.metaVars.map(_._2)).nonEmpty
  }


  override def update(o: Any, n: Any): Boolean = {
    leo.Out.warn("Updating substitutions is not supported.")
    false
  }
  override def insert(n: Any): Boolean = n match {
    case s : Map[Int, Term] =>
      synchronized(sub.comp(Subst.fromMap(s)))
      metaMap ++= s
      true
    case _ => false
  }
  override def clear(): Unit = synchronized{
    sub = Subst.id
    metaMap.clear()
  }
  override def all(t: DataType): Set[Any] = t match{
    case SubstitutionType => Set(sub)
    case _ => Set()
  }
  override def delete(d: Any): Unit = {
    leo.Out.warn("Changing the substitution is not supported.")
  }
}

case object SubstitutionType extends DataType
