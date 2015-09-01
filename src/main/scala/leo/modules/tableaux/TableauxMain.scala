package leo.modules.tableaux


import leo.datastructures.Term.MetaVar
import leo.datastructures._
import leo.datastructures.impl.Signature
import leo.modules.tableaux.unification.FOUnification

/**
 * Created by max on 01.09.2015.
 */
object TableauxMain {
  def main(args : Array[String]) {
    val s : Signature = Signature.get

    val m1 : Term = Term.mkFreshMetaVar(s.o)
    val m2 : Term = Term.mkFreshMetaVar(s.o)

    val t : Term = &(m1 , m2)

    val t2 : Term = &(LitTrue, LitFalse)

    println(t.pretty)

    println(s"Unifying ${t.pretty} and ${t2.pretty}")
    val sub1 : Option[Subst] = FOUnification(t)(t2)
    println("Unifier : "+sub1.fold("No Unifier")(_.pretty))
    sub1.map{s => println(t.substitute(s).betaNormalize.pretty + " = " + t2.substitute(s).betaNormalize.pretty)}
  }
}
