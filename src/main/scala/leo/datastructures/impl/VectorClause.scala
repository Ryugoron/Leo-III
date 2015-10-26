package leo.datastructures.impl

import leo.datastructures._


// TODO
// Keep meta vars, transform to bound? transform bound to meta?
/**
 * Preliminary implementation of clauses using indexed linear sequences (vectors).
 *
 * @author Alexander Steen
 * @since 23.11.2014
 */
 abstract sealed class VectorClause extends Clause {
  /** The types of the implicitly universally quantified variables. */
  final val implicitlyBound: Set[(Type, Int)] = lits.map(_.metaVars).toSet.flatten

  /** Those literals in `lits` that are positive. */
  final val posLits: Seq[Literal] = lits.filter(_.polarity)
  /** Those literals in `lits` that are negative. */
  final val negLits: Seq[Literal] = lits.filter(!_.polarity)
}

object VectorClause {
  private var clauseCounter : Int = 0

  final def mkClause(lits: Iterable[Literal], origin: ClauseOrigin): Clause = {
    println("constructor")
    clauseCounter += 1
    try {
      new VectorClause0(clauseCounter, lits, origin)
    } catch {
      case e: Exception => println(e.toString); e.printStackTrace(); throw e
    }
  }

  @inline final def lastClauseId = clauseCounter

  private class VectorClause0(val id: Int, literals: Iterable[Literal], val origin: ClauseOrigin) extends VectorClause {
    lazy val lits = literals.toVector
  }
}
