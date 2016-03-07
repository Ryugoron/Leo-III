package leo.modules.seqpproc

import leo.Configuration
import leo.Out
import leo.datastructures.impl.Signature
import leo.datastructures.{Term, Clause, Literal, Role, Role_Axiom, Role_Definition, Role_Type, Role_Conjecture, Role_NegConjecture, Role_Plain, Pretty, LitTrue, LitFalse, ClauseAnnotation, ClauseProxy}
import ClauseAnnotation._
import leo.modules.output._
import leo.modules.{Utility, SZSOutput, SZSException, Parsing}
import leo.modules.calculus.{Subsumption, CalculusRule}
import leo.modules.seqpproc.controlStructures._

import scala.collection.SortedSet

/**
 * Created by lex on 10/28/15.
 */
object SeqPProc extends Function1[Long, Unit]{

  private final def termToClause(t: Term): Clause = {
    Clause.mkClause(Seq(Literal.mkLit(t, true)))
  }

  final def preprocess(cur: ClauseWrapper): Set[ClauseWrapper] = {
    val cl = cur.cl
    // Fresh clause, that means its unit and nonequational
    assert(Clause.unit(cl), "clause not unit")
    val lit = cl.lits.head
    var pol = lit.polarity
    assert(!lit.equational, "initial literal equational")
    var cw = cur
    var left = lit.left

    // Def expansion and simplification
    left = DefExpSimp(left)
    cw = ClauseWrapper(Clause(Literal(left, pol)), InferredFrom(DefExpSimp, Set(cw)))
    Out.trace(s"Def expansion: ${cw.cl.pretty}")

    if (PolaritySwitch.canApply(left)) {
      left = PolaritySwitch(left)
      pol = !pol
      cw = ClauseWrapper(Clause(Literal(left, pol)), InferredFrom(PolaritySwitch, Set(cw)))
      Out.trace(s"Pol. switch: ${cw.cl.pretty}")
    }

    // Exhaustively CNF
    val left2 = CNF(leo.modules.calculus.freshVarGen(cw.cl), cw.cl).map(Simp.shallowSimp).map(ClauseWrapper(_, InferredFrom(CNF, Set(cw)))).toSet
    Out.trace(s"CNF:\n\t${left2.map(_.pretty).mkString("\n\t")}")

    // Remove defined equalities as far as possible
    val leftEq = Control.convertDefinedEqualities(left2)

    val left3 = leftEq.map { c =>
      // To equation if possible
      var cur_c = c
      val (cA_lift, lift, lift_other) = LiftEq.canApply(c.cl)
      if (cA_lift) {
        val curr = Clause(LiftEq(lift, lift_other))
        Out.trace(s"to_eq: ${curr.pretty}")
        cur_c = ClauseWrapper(curr, InferredFrom(LiftEq, Set(c)))
      }
      val (cA_funcExt, fE, fE_other) = FuncExt.canApply(cur_c.cl)
      if (cA_funcExt) {
          Out.trace(s"Func Ext on: ${cur_c.pretty}")
          val funcExt_cw = ClauseWrapper(Clause(FuncExt(leo.modules.calculus.freshVarGen(cur_c.cl),fE) ++ fE_other), InferredFrom(FuncExt, Set(cur_c)))
          Out.finest(s"Func Ext result: ${funcExt_cw.pretty}")
          cur_c = funcExt_cw
      }
      cur_c
    }

    // Do here AC and EQ Simp
    val leftAC = if (Configuration.isSet("acsimp")) {
      val acSymbols = Signature.get.acSymbols
      val a = left3.map {c => Out.trace(s"AC Simp on ${c.pretty}");val res = ACSimp.apply(c.cl,acSymbols);Out.trace(s"AC Result: ${res.pretty}");ClauseWrapper(res, InferredFrom(ACSimp, Set(c)))}
      a.map {c => Out.trace(s"Shallow Simp on ${c.pretty}");val res = Simp.shallowSimp(c.cl);Out.trace(s"Simp Result: ${res.pretty}");ClauseWrapper(res, InferredFrom(Simp, Set(c)))}
    }
    else left3


//    val left4 = if (Configuration.isSet("nbe")) leftAC
//    else leftAC union leftAC.flatMap { c =>
//      val (cA_boolExt, bE, bE_other) = BoolExt.canApply(c.cl)
//      if (cA_boolExt) {
//        Out.trace(s"Bool Ext on: ${c.pretty}")
//        val boolExt_cws = BoolExt.apply(bE, bE_other).map(ClauseWrapper(_, InferredFrom(BoolExt, Set(c))))
//        Out.finest(s"Bool Ext result:\n\t${boolExt_cws.map(_.pretty).mkString("\n\t")}")
//        boolExt_cws.flatMap(cw => {Out.finest(s"#\ncnf of ${cw.pretty}:\n\t");CNF(leo.modules.calculus.freshVarGen(cw.cl),cw.cl)}.map(c => {val res = ClauseWrapper(c, InferredFrom(CNF, Set(cw))); Out.finest(s"${res.pretty}\n\t"); res}))
//      } else
//        Set(c)
//    }
    val left4 = leftAC

    val left5 = left4.map(cw => {Out.trace(s"Simp on ${cw.id}");val res = ClauseWrapper(Simp(cw.cl), InferredFrom(Simp, Set(cw)));Out.trace(s"Simp result: ${res.pretty}");res})
    val left6 = left5.filterNot(cw => Clause.trivial(cw.cl))

    // Pre-unify new clauses
    val left7 = Control.preunifySet(left6)

    // TODO: Do that in a reasonable way...
    left7.foreach(_.properties = ClauseAnnotation.PropUnified)
    left7
  }



  final def simplify(cw: ClauseWrapper, rules: Set[ClauseWrapper]): ClauseWrapper = {
    val simp = Simp.shallowSimp(cw.cl)
    val rewriteSimp = RewriteSimp.apply(rules.map(_.cl), simp)
    // TODO: simpl to be simplification by rewriting à la E etc
    if (rewriteSimp != cw.cl) ClauseWrapper(rewriteSimp, InferredFrom(RewriteSimp, Set(cw)))
    else cw
  }


  ///////////////////////////////////////////////////////////

  /* Main function containing proof loop */
  final def apply(startTime: Long): Unit = {

    // Read problem
    val input = Parsing.parseProblem(Configuration.PROBLEMFILE)
    val startTimeWOParsing = System.currentTimeMillis()
    // Filter out inputs that were produced by definitions and type declarations
    val filteredInput = input.filterNot(i => i._3 == Role_Definition || i._3 == Role_Type)
    // Negate conjecture
    val conjecture = filteredInput.filter {case (id, term, role) => role == Role_Conjecture}
    if (conjecture.size > 1) throw new SZSException(SZS_InputError, "At most one conjecture per input problem permitted.")

    val effectiveInput: Seq[ClauseWrapper] = if (conjecture.isEmpty) {
      filteredInput.map { case (id, term, role) => ClauseWrapper(id, termToClause(term), role, FromFile(Configuration.PROBLEMFILE, id), ClauseAnnotation.PropNoProp) }
    } else {
      assert(conjecture.size == 1)
      val conj = conjecture.head
      val conjWrapper = ClauseWrapper(conj._1, Clause.mkClause(Seq(Literal.mkLit(conj._2, true))), conj._3, FromFile(Configuration.PROBLEMFILE, conj._1), ClauseAnnotation.PropNoProp)
      val rest = filteredInput.filterNot(_._1 == conjecture.head._1)
      rest.map { case (id, term, role) => ClauseWrapper(id, termToClause(term), role, FromFile(Configuration.PROBLEMFILE, id), ClauseAnnotation.PropNoProp) } :+ ClauseWrapper(conj._1 + "_neg", Clause.mkClause(Seq(Literal.mkLit(conj._2, false))), Role_NegConjecture, InferredFrom(new CalculusRule {
        override def name: String = "neg_conjecture"
        override val inferenceStatus = Some(SZS_CounterSatisfiable)
      }, Set(conjWrapper)),ClauseAnnotation.PropNoProp)
    }

    // Proprocess terms with standard normalization techniques for terms (non-equational)
    // transform into equational literals if possible
    val state: State[ClauseWrapper] = State.fresh(Signature.get)
    Out.debug("## Preprocess BEGIN")
    val inputIt = effectiveInput.iterator
    while (inputIt.hasNext) {
      val cur = inputIt.next()
      Out.debug(s"# Process: ${cur.pretty}")
      val processed = preprocess(cur)
      Out.debug(s"# Result:\n\t${processed.map{_.pretty}.mkString("\n\t")}")
      var preprocessed = processed.filterNot(cw => Clause.trivial(cw.cl))
      state.addUnprocessed(preprocessed)
      if (inputIt.hasNext) Out.trace("--------------------")
    }
    Out.debug("## Preprocess END\n\n")

    val preprocessTime = System.currentTimeMillis() - startTimeWOParsing
    Control.fvIndexInit(state.unprocessed.toSet)
    var loop = true

    // proof loop
    Out.debug("## Reasoning loop BEGIN")
    Out.debug(s"${state.noProcessedCl}")
    while (loop && !prematureCancel(state.noProcessedCl)) {
      if (System.currentTimeMillis() - startTime > 1000*Configuration.TIMEOUT) {
        loop = false
        state.setSZSStatus(SZS_Timeout)
      } else if (state.unprocessed.isEmpty) {
        loop = false
      } else {
        // No cancel, do reasoning step
        var cur = state.nextUnprocessed
        // cur is the current clausewrapper
        Out.debug(s"Taken: ${cur.pretty}")

        cur = simplify(cur, state.rewriteRules)
        if (Clause.effectivelyEmpty(cur.cl)) {
          loop = false
          if (conjecture.isEmpty) {
            state.setSZSStatus(SZS_ContradictoryAxioms)
          } else {
            state.setSZSStatus(SZS_Theorem)
          }
          state.setDerivationClause(cur)
        } else {
          // Subsumption
          if (!state.processed.exists(cw => Subsumption.subsumes(cw.cl, cur.cl))) {
            var newclauses: Set[ClauseWrapper] = Set()

            /////////////////////////////////////////
            // Simplifying (mofifying inferences and backward subsumption) BEGIN
            // TODO: à la E: direct descendant criterion, etc.
            /////////////////////////////////////////
            /* Subsumption */
            state.setProcessed(state.processed.filterNot(cw => Subsumption.subsumes(cur.cl, cw.cl)))
            state.addProcessed(cur)
            Control.fvIndexInsert(cur)
            /* Add rewrite rules to set */
            if (Clause.rewriteRule(cur.cl)) {
              state.addRewriteRule(cur)
            }
            /* Functional Extensionality */
            val (cA_funcExt, fE, fE_other) = FuncExt.canApply(cur.cl)
            if (cA_funcExt) {
              Out.debug(s"Func Ext on: ${cur.pretty}")
              val funcExt_cw = ClauseWrapper(Clause(FuncExt(leo.modules.calculus.freshVarGen(cur.cl),fE) ++ fE_other), InferredFrom(FuncExt, Set(cur)))
              Out.trace(s"Func Ext result: ${funcExt_cw.pretty}")
              cur = funcExt_cw
            }
            /* To equality if possible */
            val (cA_lift, lift, lift_other) = LiftEq.canApply(cur.cl)
            if (cA_lift) {
              val newCl = Clause(LiftEq(lift, lift_other))
              cur = ClauseWrapper(newCl, InferredFrom(LiftEq, Set(cur)))
              // No break here
            }
            /////////////////////////////////////////
            // Simplifying (mofifying inferences) END
            /////////////////////////////////////////

            /////////////////////////////////////////
            // Generating inferences BEGIN
            /////////////////////////////////////////
            /* Boolean Extensionality */
            val boolext_result = Control.boolext(cur)
            newclauses = newclauses union boolext_result

            /* paramodulation where at least one involved clause is `cur` */
            val paramod_result = Control.paramodSet(cur, state.processed)
            newclauses = newclauses union paramod_result

            /* Equality factoring of `cur` */
            val factor_result = Control.factor(cur)
            newclauses = newclauses union factor_result

            /* Prim subst */
            val primSubst_result = Control.primsubst(cur)
            newclauses = newclauses union primSubst_result

            /* TODO: Choice */
            /////////////////////////////////////////
            // Generating inferences END
            /////////////////////////////////////////

            /////////////////////////////////////////
            // Simplification of newly generated clauses BEGIN
            /////////////////////////////////////////
            /* Simplify new clauses */
            newclauses = newclauses.map(cw => {Out.trace(s"Simp on ${cw.pretty}");val res = ClauseWrapper(Simp(cw.cl), InferredFrom(Simp, Set(cw)));Out.trace(s"Simp result: ${res.pretty}");res})
            /* Remove those which are tautologies */
            newclauses = newclauses.filterNot(cw => Clause.trivial(cw.cl))
            /* exhaustively CNF new clauses */
            newclauses = newclauses.flatMap(cw => {Out.finest(s"#####################\ncnf of ${cw.pretty}:\n\t");CNF(leo.modules.calculus.freshVarGen(cw.cl),cw.cl)}.map(c => {val res = ClauseWrapper(c, InferredFrom(CNF, Set(cw)));Out.finest(s"${res.pretty}\n\t") ;res}))

            /* Pre-unify new clauses */
            val preuni_result = Control.preunifySet(newclauses)
            newclauses = preuni_result

            /* Replace defined equalities */
            val defEq_result = Control.convertDefinedEqualities(newclauses)
            newclauses = defEq_result

            /* Replace eq symbols on top-level by equational literals. */
            newclauses = newclauses.map { c =>
              val (cA_lift, lift, lift_other) = LiftEq.canApply(c.cl)
              if (cA_lift) {
                val curr = Clause(LiftEq(lift, lift_other))
                Out.trace(s"to_eq: ${curr.pretty}")
                ClauseWrapper(curr, InferredFrom(LiftEq, Set(c)))
              } else c
            }
            /////////////////////////////////////////
            // Simplification of newly generated clauses END
            /////////////////////////////////////////

            /////////////////////////////////////////
            // At the end, for each generated clause apply simplification etc.
            // and add to unprocessed
            /////////////////////////////////////////
            val newIt = newclauses.iterator
            while (newIt.hasNext) {
              var newCl = newIt.next()
              // Simplify again, including rewriting etc.
              newCl = simplify(newCl, state.rewriteRules)

              if (!Clause.trivial(newCl.cl)) {
                state.incGeneratedCl(1)
                state.addUnprocessed(newCl)
              } else {
                Out.trace(s"Trivial, hence dropped: ${newCl.pretty}")
              }
            }

          } else {
            Out.debug("clause subsumbed, skipping.")
            state.incForwardSubsumedCl()
            Out.trace(s"Subsumed by:\n\t${state.processed.filter(cw => Subsumption.subsumes(cw.cl, cur.cl)).map(_.pretty).mkString("\n\t")}")
          }

        }
      }

    }

    @inline def prematureCancel(counter: Int): Boolean = {
      try {
        val limit: Int = Configuration.valueOf("ll").get.head.toInt
        counter >= limit
      } catch {
        case e: NumberFormatException => false
        case e: NoSuchElementException => false
      }
    }

    /////////////////////////////////////////
    // Main loop terminated, print result
    /////////////////////////////////////////

    val time = System.currentTimeMillis() - startTime
    val timeWOParsing = System.currentTimeMillis() - startTimeWOParsing

    Out.output("")
    Out.output(SZSOutput(state.szsStatus, Configuration.PROBLEMFILE, s"${time} ms resp. ${timeWOParsing} ms w/o parsing"))

    /* Output additional information about the reasoning process. */
    Out.comment(s"Time passed: ${time}ms")
    Out.comment(s"Effective reasoning time: ${timeWOParsing}ms")
    Out.comment(s"Thereof preprocessing: ${preprocessTime}ms")
    Out.comment(s"No. of processed clauses: ${state.noProcessedCl}")
    Out.comment(s"No. of generated clauses: ${state.noGeneratedCl}")
    Out.comment(s"No. of forward subsumed clauses: ${state.noForwardSubsumedCl}")
    Out.comment(s"No. of units in store: ${state.rewriteRules.size}")
    if (Out.logLevelAtLeast(java.util.logging.Level.FINEST)) {
      Out.comment("Signature extension used:")
      Out.comment(s"Name\t|\tId\t|\tType/Kind\t|\tDef.\t|\tProperties")
      Out.comment(Utility.userDefinedSignatureAsString) // TODO: Adjust for state
    }
    // FIXME: Count axioms used in proof:
    //    if (derivationClause != null)
    //      Out.output(s" No. of axioms used: ${axiomsUsed(derivationClause)}")

    /* Print proof object if possible and requested. */
    if (state.szsStatus == SZS_Theorem && Configuration.PROOF_OBJECT && state.derivationClause.isDefined) {
      Out.comment(s"SZS output start CNFRefutation for ${Configuration.PROBLEMFILE}")
//      Out.output(makeDerivation(derivationClause).drop(1).toString)
      Utility.printDerivation(state.derivationClause.get)
      Out.comment(s"SZS output end CNFRefutation for ${Configuration.PROBLEMFILE}")
    }
  }


  def makeDerivation(cw: ClauseProxy, sb: StringBuilder = new StringBuilder(), indent: Int = 0): StringBuilder = cw.annotation match {
    case NoAnnotation => sb.append("\n"); sb.append(" ` "*indent); sb.append(s"thf(${cw.id}, ${cw.role}, ${cw.cl.pretty}).")
    case a@FromFile(_, _) => sb.append("\n"); sb.append(" ` "*indent); sb.append(s"thf(${cw.id}, ${cw.role}, ${cw.cl.pretty}, ${a.pretty}).")
    case a@InferredFrom(_, parents) => {
      sb.append("\n");
      sb.append(" | "*indent);
      sb.append(s"thf(${cw.id}, ${cw.role}, ${cw.cl.pretty}, ${a.pretty}).")
      if (parents.size == 1) {
        makeDerivation(parents.head._1,sb,indent)
      } else parents.foreach {case (parent, _) => makeDerivation(parent,sb,indent+1)}
      sb
    }
  }
}



