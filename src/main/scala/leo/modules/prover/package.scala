package leo.modules

import leo.datastructures.ClauseAnnotation.InferredFrom
import leo.{Configuration, Out}
import leo.datastructures.{AnnotatedClause, ClauseAnnotation, Term, tptp}
import leo.modules.calculus.NegateConjecture
import leo.modules.control.Control
import leo.modules.output.StatusSZS
import leo.modules.output.{SZS_InputError, SZS_Theorem, SZS_TypeError, SZS_Unsatisfiable, SZS_CounterSatisfiable, SZS_ContradictoryAxioms}
import leo.modules.parsers.Input

import scala.annotation.tailrec

/**
  * Created by lex on 26.05.17.
  */
package object prover {
  type LocalGeneralState = GeneralState[AnnotatedClause]
  type LocalState = State[AnnotatedClause]

  ////////////////////////////////////
  //// Loading and converting the problem
  ////////////////////////////////////

  /** Converts the input into clauses and filters the axioms if applicable. */
  final def effectiveInput(input: Seq[tptp.Commons.AnnotatedFormula], state: LocalGeneralState): Seq[AnnotatedClause] = {
    Out.info(s"Parsing finished. Scanning for conjecture ...")
    val (effectiveInput,conj) = effectiveInput0(input, state)
    if (state.negConjecture != null) {
      Out.info(s"Found a conjecture and ${effectiveInput.size} axioms. Running axiom selection ...")
      // Do relevance filtering: Filter hopefully unnecessary axioms
      val relevantAxioms = if (effectiveInput.size <= 15) effectiveInput
                            else Control.getRelevantAxioms(effectiveInput, conj)(state.signature)
      Out.info(s"Axiom selection finished. Selected ${relevantAxioms.size} axioms " +
        s"(removed ${effectiveInput.size - relevantAxioms.size} axioms).")
      relevantAxioms.map(ax => processInput(ax, state))
    } else {
      Out.info(s"${effectiveInput.size} axioms and no conjecture found.")
      effectiveInput.map(ax => processInput(ax, state))
    }
  }

  /** Insert types, definitions and the conjecture to the signature resp. state. The rest
    * (axioms etc.) is left unchanged for relevance filtering. Throws an error if multiple
    * conjectures are present or unknown role occurs. */
  final private def effectiveInput0(input: Seq[tptp.Commons.AnnotatedFormula], state: LocalGeneralState): (Seq[tptp.Commons.AnnotatedFormula], tptp.Commons.AnnotatedFormula) = {
    import leo.datastructures.{Role_Definition, Role_Type, Role_Conjecture, Role_NegConjecture, Role_Unknown}
    import leo.datastructures.ClauseAnnotation._
    var result: Seq[tptp.Commons.AnnotatedFormula] = Vector()
    var conj: tptp.Commons.AnnotatedFormula = null
    val inputIt = input.iterator
    while (inputIt.hasNext) {
      val formula = inputIt.next()
      formula.role match {
        case Role_Type.pretty => Input.processFormula(formula)(state.signature)
        case Role_Definition.pretty => Control.relevanceFilterAdd(formula)(state.signature)
          Input.processFormula(formula)(state.signature)
        case Role_Conjecture.pretty =>
          if (state.negConjecture == null) {
            if (Configuration.CONSISTENCY_CHECK) {
              Out.info(s"Input conjecture ignored since 'consistency-only' is set.")
              /* skip */
            } else {
              // Convert and negate and add conjecture
              Control.relevanceFilterAdd(formula)(state.signature)
              val translated = Input.processFormula(formula)(state.signature)
              val conjectureClause = AnnotatedClause(termToClause(translated._2), Role_Conjecture, FromFile(Configuration.PROBLEMFILE, translated._1), ClauseAnnotation.PropNoProp)
              state.setConjecture(conjectureClause)
              val negConjectureClause = AnnotatedClause(termToClause(translated._2, false), Role_NegConjecture, InferredFrom(NegateConjecture, conjectureClause), ClauseAnnotation.PropSOS)
              state.setNegConjecture(negConjectureClause)
              conj = formula
            }
          } else throw new SZSException(SZS_InputError, "At most one conjecture per input problem is permitted.")
        case Role_NegConjecture.pretty =>
          if (state.negConjecture == null) {
            if (Configuration.CONSISTENCY_CHECK) {
              Out.info(s"Input conjecture ignored since 'consistency-only' is set.")
              /* skip */
            } else {
              Control.relevanceFilterAdd(formula)(state.signature)
              val translated = Input.processFormula(formula)(state.signature)
              val negConjectureClause = AnnotatedClause(termToClause(translated._2), Role_NegConjecture, FromFile(Configuration.PROBLEMFILE, translated._1), ClauseAnnotation.PropSOS)
              state.setNegConjecture(negConjectureClause)
              conj = formula
            }
          } else throw new SZSException(SZS_InputError, "At most one (negated) conjecture per input problem is permitted.")
        case Role_Unknown.pretty =>
          throw new SZSException(SZS_InputError, s"Formula ${formula.name} has role 'unknown' which is regarded an error.")
        case _ =>
          Control.relevanceFilterAdd(formula)(state.signature)
          result = formula +: result
      }
    }
    (result,conj)
  }

  final private def processInput(input: tptp.Commons.AnnotatedFormula, state: LocalGeneralState): AnnotatedClause = {
    import leo.datastructures.ClauseAnnotation.FromFile
    val formula = Input.processFormula(input)(state.signature)
    AnnotatedClause(termToClause(formula._2), formula._3, FromFile(Configuration.PROBLEMFILE, formula._1), ClauseAnnotation.PropNoProp)
  }

  ////////////////////////////////////
  //// Further Utility
  ////////////////////////////////////
  final def typeCheck(input: Seq[AnnotatedClause], state: LocalGeneralState): Unit = {
    if (state.negConjecture != null) typeCheck0(state.negConjecture +: input, state)
    else typeCheck0(input, state)
  }
  @tailrec final private def typeCheck0(input: Seq[AnnotatedClause], state: LocalGeneralState): Unit = {
    import leo.datastructures.ClauseAnnotation.FromFile
    import leo.modules.HOLSignature.o
    if (input.nonEmpty) {
      val hd = input.head
      val term = hd.cl.lits.head.left
      val annotation = if (hd.annotation.isInstanceOf[FromFile]) hd.annotation.asInstanceOf[FromFile]
      else hd.annotation.parents.head.annotation.asInstanceOf[FromFile]

      if (!Term.wellTyped(term)) {
        leo.Out.severe(s"Input problem did not pass type check: formula '${annotation.formulaName}' is ill-typed.")
        throw new SZSException(SZS_TypeError, s"Type error in formula '${annotation.formulaName}' from file '${annotation.fileName}'.")
      } else if (term.ty != o) {
        leo.Out.severe(s"Input problem did not pass type check: '${annotation.formulaName}' is not Boolean typed.")
        throw new SZSException(SZS_TypeError, s"Term of non-Boolean type at top-level in formula '${annotation.formulaName}' from file '${annotation.fileName}'.")
      } else {
        typeCheck0(input.tail, state)
      }
    }
  }

  final def endplay(emptyClause: AnnotatedClause, state: LocalState): Unit = {
    state.setDerivationClause(emptyClause)
    val proof = proofOf(emptyClause)
    state.setProof(proof)

    if (state.conjecture == null) state.setSZSStatus(SZS_Unsatisfiable)
    else {
      if (conjInProof(proof)) state.setSZSStatus(SZS_Theorem)
      else state.setSZSStatus(SZS_ContradictoryAxioms)
    }
  }

  final protected[prover] def endgameAnswer(result: StatusSZS): Boolean = {
    result match {
      case SZS_CounterSatisfiable | SZS_Theorem | SZS_Unsatisfiable => true
      case _ => false
    }
  }

  def extCallInference(prover: String, source: Set[AnnotatedClause]): ClauseAnnotation = {
    InferredFrom(new leo.modules.calculus.CalculusRule {
      final val name: String = prover
      final val inferenceStatus = SZS_Theorem
    }, source.toSeq)
  }
}
