//package leo.datastructures.impl.orderings
//
//import leo._
//import leo.datastructures.{ClauseProxy, Literal}
//
/////////////////////////////////
//// Literal weights
/////////////////////////////////
//
///** Simple weighting function that gives every literal the same weight. */
//object LW_Constant extends LiteralWeight {
//  def weightOf(lit: Literal): Int = 1
//}
//
///** Literal weighting that uses the enclosed term's size as weight. */
//object LW_TermSize extends LiteralWeight {
//  def weightOf(lit: Literal): Int = Literal.asTerm(lit).size
//}
//
//// more to come ...
//
///////////////////////////////////
//// Clause proxy weights
///////////////////////////////////
//
///** Weighting that gives a higher ('worse') weight for newer clauses. */
//object CPW_FIFO extends ClauseProxyWeight {
//  def weightOf(cl: ClauseProxy): Int = cl.id.toInt // FIXME Long to Int conversion
//}
//
///** Clause weighting that assigns the number of literals in the clause as weight. */
//object CPW_LitCount extends ClauseProxyWeight {
//  def weightOf(cl: ClauseProxy): Int = cl.cl.lits.size
//}
//
///** Clause weighting that assigns the maximum of all literals weights as weight. */
//object CPW_MaxLitWeight extends ClauseProxyWeight {
//  def weightOf(cl: ClauseProxy): Int = cl.cl.lits.map(_.weight).max
//}
//
///** Clause weighting that assigns the sum of all literals weights as weight. */
//object CPW_LitWeightSum extends ClauseProxyWeight {
//  def weightOf(cl: ClauseProxy): Int = cl.cl.lits.map(_.weight).sum
//}