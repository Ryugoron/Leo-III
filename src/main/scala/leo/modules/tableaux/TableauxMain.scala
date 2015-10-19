package leo.modules.tableaux


import leo.datastructures.Term.MetaVar
import leo.datastructures._
import leo.datastructures.impl.Signature
import leo.modules.tableaux.datastructures.SubstitionState
import leo.modules.tableaux.unification.FOUnification

/**
 * Created by max on 01.09.2015.
 */
object TableauxMain {
  def main(args : Array[String]) {
    val s : Signature = Signature.get

    val m1 : Term = Term.mkFreshMetaVar(s.o)
    println(m1)
    val m2 : Term = Term.mkFreshMetaVar(s.o)
    val m3 : Term = Term.mkFreshMetaVar(s.o)

    val t : Term = &(m2 , &(m1, m3))

    val t2 : Term = &(m1, &(m2, LitFalse()))

    println(t.pretty)

    println(s"Unifying ${t.pretty} and ${t2.pretty}")
    val sub1 : Option[Map[Int, Term]] = FOUnification(t)(t2)
    val sublsub : Option[Subst] = sub1 map {s => Subst.fromMap(s)}
    println("Unifier : "+sublsub.fold("No Unifier")(_.pretty))
    sublsub.map{s => println(t.substitute(s).betaNormalize.pretty + " = " + t2.substitute(s).betaNormalize.pretty)}
    println("Print single unifier")
    var i : Int = 0
    sublsub.map{s =>
      s.normalize.fronts.foreach{f =>
        i += 1
        println(s"$i subst : ${f.pretty}")
      }
    }

    println("Print single unifier")
    sub1.map{s => s.foreach{ case (index, term) =>
        println(s"$index -> ${term.pretty}")
      }
    }

  }
}
