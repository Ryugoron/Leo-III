package leo

import leo.datastructures.internal.Term
import leo.datastructures.internal.Term.{mkTermApp => ap,mkAtom}
import leo.datastructures.internal.{LitFalse, LitTrue, === => EQUALS, Signature}

import leo.modules.churchNumerals.Numerals
import leo.modules.churchNumerals.Numerals.fromInt

import leo.modules.normalization.Simplification
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite

/**
 * Created by lex on 11.06.14.
 */
@RunWith(classOf[JUnitRunner])
class ExecutionTest extends FunSuite {
  val sig = Signature.get

  Numerals() // include numerals in signature


  val theorems: Map[String, Term] = {
    val add = mkAtom(sig.meta("add").key)
    val mult = mkAtom(sig.meta("mult").key)
    val power = mkAtom(sig.meta("power").key)

    Map(("2+3=3+2", {
      EQUALS(ap(ap(add,2),3), ap(ap(add,3),2))
    }),
      ("2*(1+2) = 6", {
        EQUALS(ap(ap(mult,2),ap(ap(add,1),2)), 6)
      }),
      ("2^3 = 2*2*2",{
        EQUALS(ap(ap(power,2),3), ap(ap(mult,ap(ap(mult,2),2)),2))
      }))
  }


  for((s,t) <- theorems) {
    test(s) {
      val res = runExecutionOn(s,t)
      res match {
        case LitTrue() => {assert(true); println("THEOREM")}
        case LitFalse() => {assert(false); println("COUNTER PROVABLE")}
        case _ => {assert(false); println("UNKNOWN")}
      }
      println("############################################")
    }

  }

  def runExecutionOn(title: String, term: Term): Term = {
    println("To prove: " + title)
    println()
    println("As term: " + term.pretty)
    println()
    println("Definition expansion ...")
    val test2 = term.expandAllDefinitions
    //println(test2.pretty)

    println("Beta normalizing ...")
    val test3 = test2.betaNormalize
    //println(test3.pretty)

    println("Simplification ...")
    val test4 = Simplification(test3)

    println(" (Resulting term: " + test4.pretty + " )")
    test4
  }
}
