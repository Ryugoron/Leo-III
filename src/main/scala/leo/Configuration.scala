package leo

import java.util.logging.Level
import java.nio.file.{Path,Files}

import leo.modules.output.Output
import leo.modules.parsers.CLParameterParser

/**
 * Configuration access point where central parameter settings of Leo-III
 * can be accessed and read. The Configuration object needs to be initialized
 * using the `init` method and a command line argument parser, i.e. subclasses of
 * `CLParameterParser`,
 *
 * @author Alexander Steen
 * @since 13.11.2014
 */
object Configuration extends DefaultConfiguration {
  private var configMap: Map[String, Seq[String]] = _

  private val PARAM_THREADCOUNT = "n"
  private val PARAM_VERBOSITY = "v"
  private val PARAM_TIMEOUT = "t"
  private val PARAM_PROOFOBJECT = "p"
  private val PARAM_HELP = "h"
  private val PARAM_COUNTERSAT = "c"
  private val PARAM_SOS_SHORT = "s"
  private val PARAM_SOS_LONG = "sos"
  private val PARAM_UNIFICATIONDEPTH = "unidepth"
  private val PARAM_UNIFIERCOUNT = "unifiers"
  private val PARAM_MATCHINGDEPTH = "matchingdepth"
  private val PARAM_PRIMSUBST = "primsubst"
  private val PARAM_RESTRICTUNIATTEMPTS = "restrict-uni-attempts"
  private val PARAM_PRE_PRIMSUBST = "instantiate"
  private val PARAM_PRE_PRIMSUBST_MAXDEPTH = "instantiate-maxdepth"
  private val PARAM_FUNCSPEC = "funcspec"
  private val PARAM_RELEVANCEFILTER = "relevancefiltering"
  private val PARAM_PASSMARK = "passmark"
  private val PARAM_AGING = "aging"
  private val PARAM_NOCHOICE = "nochoice"
  private val PARAM_NOAXIOMSELECTION = "noaxiomselection"
  private val PARAM_ATPCALLINTERVAL = "atp-call-interval"
  private val PARAM_ATPMAXJOBS = "atp-max-jobs"
  private val RENAMING = "renaming"
  private val PARAM_CONSISTENCYCHECK = "consistency-only"
  private val EXTRACTION_TYPE_PARAM = "xType"
  private val PARAM_PAR_SCHED = "parSched"
  private val PARAM_CONCURRENT_TRANSLATE = "encode-threaded"
  private val PARAM_GUIDED = "guided"

  // Collect standard options for nice output: short-option -> (long option, argname, description)
  private val optionsMap : Map[Char, (String, String, String)] = {
    Map(
      'h' -> ("", "", "Display this help message"),
      'n' -> ("", "N", "Maximum number of threads"),
      'p' -> ("", "", "Display proof output"),
      't' -> ("", "N", "Timeout in seconds"),
      'v' -> ("", "Lvl", "Set verbosity: From 0 (No Logging output) to 6 (very fine-grained debug output)"),
      'c' -> ("", "Csat", "Sets the proof mode to counter satisfiable (Through remote proof"),
      's' -> ("sos", "", "Use SOS heuristic search strategy"),
      'a' -> ("atp", "name=call", "Addition of external provers"),
      'e' -> ("atp-timout", "name=N", "Timeout for an external prover in seconds."),
      'x' -> ("atp-args", "name=\"args\"", "Arguments directly passed to the external prover.")
    )
  }

  /////////////////////////

  def init(parameterParser: CLParameterParser): Unit = configMap match {
    case null =>
      configMap = Map()
      for(param <- parameterParser.getParameters) {
        configMap += param
      }
      // Force computation of lazy values for early error output
      PROBLEMFILE
      THREADCOUNT
      TIMEOUT
      PROOF_OBJECT
      VERBOSITY
      COUNTER_SAT
      SOS
      ATPS
      HELP
      ()
    case _ => ()
  }

  final def cleanup(): Unit = {
    leo.Out.debug(s"Cleaning up temporary files ...")
    import leo.modules.external.ExternalProver
    ExternalProver.cleanup()
    leo.Out.debug(s"Clean-up finished!")
  }

  //////////////////////////
  // Predefined parameters
  //////////////////////////
  def isInit: Boolean = configMap != null

  final val VERSION: String = "1.1"
  final val LEODIR_NAME: String = "leo3"
  final lazy val LEODIR: Path = {
    val dir = Files.createTempDirectory(LEODIR_NAME)
    dir.toFile.deleteOnExit()
    dir
  }

  lazy val HELP: Boolean = isSet(PARAM_HELP)

  lazy val PROBLEMFILE: String = configMap.get(CLParameterParser.ARG0Name) match {
    case None => throw new IllegalArgumentException("No problem file given. Aborting.")
    case Some(str :: Nil) => str
    case Some(_) => throw new IllegalArgumentException("This should not happen. Please call support hotline.")
  }

  lazy val THREADCOUNT: Int = uniqueIntFor(PARAM_THREADCOUNT, DEFAULT_THREADCOUNT)
  lazy val PAR_SCHED : Int = uniqueIntFor(PARAM_PAR_SCHED, DEFAULT_PAR_SCHED)

  lazy val VERBOSITY: java.util.logging.Level = {
    val v = configMap.get(PARAM_VERBOSITY) match {
      case None => DEFAULT_VERBOSITY
      case Some(arg :: Nil) => processLevel(arg)
      case Some(arg :: _) => Out.warn(multiDefOutput(PARAM_VERBOSITY))
                             processLevel(arg)
      case Some(_) => Out.warn(intExpectedOutput(PARAM_VERBOSITY,"None"))
                      DEFAULT_VERBOSITY
    }
    Out.setLogLevel(v)
    v
  }

  lazy val TIMEOUT: Int = {
    if (configMap.get(PARAM_TIMEOUT).isEmpty) Out.info(s"No timeout was given, using default timeout -t $DEFAULT_TIMEOUT")
    uniqueIntFor(PARAM_TIMEOUT, DEFAULT_TIMEOUT)
  }

  lazy val PROOF_OBJECT : Boolean = isSet(PARAM_PROOFOBJECT)

  lazy val RELEVANCE_FILTERING: Boolean = isSet(PARAM_RELEVANCEFILTER)
  lazy val RELEVANCE_PASSMARK: Double = uniqueDoubleFor(PARAM_PASSMARK, DEFAULT_PASSMARK)
  lazy val RELEVANCE_AGING: Double = uniqueDoubleFor(PARAM_AGING, DEFAULT_AGING)

  lazy val GUIDED: Boolean = isSet(PARAM_GUIDED)

  lazy val FUNCSPEC: Boolean = isSet(PARAM_FUNCSPEC) || DEFAULT_FUNCSPEC

  lazy val UNIFICATION_DEPTH: Int = uniqueIntFor(PARAM_UNIFICATIONDEPTH, DEFAULT_UNIFICATIONDEPTH)
  lazy val UNIFIER_COUNT: Int = uniqueIntFor(PARAM_UNIFIERCOUNT, DEFAULT_UNIFIERCOUNT)
  lazy val MATCHING_DEPTH: Int = uniqueIntFor(PARAM_MATCHINGDEPTH, DEFAULT_MATCHINGDEPTH)

  lazy val PRIMSUBST_LEVEL: Int = uniqueIntFor(PARAM_PRIMSUBST, DEFAULT_PRIMSUBST)
  lazy val PRE_PRIMSUBST_LEVEL: Int = uniqueIntFor(PARAM_PRE_PRIMSUBST, DEFAULT_PRE_PRIMSUBST)
  lazy val PRE_PRIMSUBST_MAX_DEPTH: Int = uniqueIntFor(PARAM_PRE_PRIMSUBST_MAXDEPTH, DEFAULT_PRE_PRIMSUBST_MAXDEPTH)

  import leo.modules.calculus.Subsumption
  lazy val SUBSUMPTION_METHOD: Subsumption = {
    if (isSet("subsumption")) {
      val method = valueOf("subsumption").get.head
      method match {
        case "ho-pattern" => leo.modules.calculus.HOPatternSubsumption
        case "trivial" => leo.modules.calculus.TrivialSubsumption
        case _ => DEFAULT_SUBSUMPTIONMETHOD
      }
    } else DEFAULT_SUBSUMPTIONMETHOD
  }

  lazy val NO_CHOICE: Boolean = isSet(PARAM_NOCHOICE) || !DEFAULT_CHOICE
  lazy val NO_AXIOM_SELECTION: Boolean = isSet(PARAM_NOAXIOMSELECTION)

  lazy val SOS: Boolean = isSet(PARAM_SOS_LONG) || isSet(PARAM_SOS_SHORT)

  lazy val RESTRICT_UNI_ATTEMPTS: Boolean = {
    if (isSet(PARAM_RESTRICTUNIATTEMPTS)) {
      val input = valueOf(PARAM_RESTRICTUNIATTEMPTS).get.head
      input match {
        case "true" => true
        case "false" => false
        case _ => DEFAULT_RESTRICTUNIATTEMPTS
      }
    } else Configuration.DEFAULT_RESTRICTUNIATTEMPTS
  }

  lazy val COUNTER_SAT : Boolean = isSet(PARAM_COUNTERSAT)
  lazy val CONSISTENCY_CHECK: Boolean = isSet(PARAM_CONSISTENCYCHECK)

  lazy val TERM_ORDERING: TermOrdering = {
    if (isSet("ordering")) {
      val ord = valueOf("ordering").get.head
      ord match {
        case "CPO" => leo.datastructures.impl.orderings.TO_CPO_Naive
        case "none" => leo.datastructures.impl.orderings.NO_ORDERING
        case _ => DEFAULT_TERMORDERING
      }
    } else{
      DEFAULT_TERMORDERING
    }
  }

  import leo.datastructures.Precedence
  lazy val PRECEDENCE: Precedence = Precedence.arityInvOrder

  lazy val RENAMING_SET : Boolean = isSet(RENAMING) || DEFAULT_RENAMING
  lazy val RENAMING_THRESHHOLD : Int = valueOf(RENAMING).fold(0)(_.headOption.fold(0)(_.toInt))
  lazy val EXTRACTION_TYPE: Int = uniqueIntFor(EXTRACTION_TYPE_PARAM, 1)

  lazy val ATP_CALL_INTERVAL: Int = uniqueIntFor(PARAM_ATPCALLINTERVAL, DEFAULT_ATPCALLINTERVAL)
  lazy val ATP_MAX_JOBS: Int = uniqueIntFor(PARAM_ATPMAXJOBS, DEFAULT_ATPMAXJOBS)
  lazy val ATPS : Seq[(String, String)] = {
    val a = valueOf("a")
    if(a.nonEmpty) {
      val atps = a.get
      atps.map{(s : String)  =>
        if(s.contains("=")) {
          val eses = s.split("=", 2)
          (eses(0), eses(1))
        } else {
          (s, "")
        }
      }
    } else {
      val b = valueOf("atp")
      if(b.nonEmpty) {
        val atps = b.get
        atps.map{(s : String)  =>
          if(s.contains("=")) {
            val eses = s.split("=", 2)
            (eses(0), eses(1))
          } else {
            (s, "")
          }
        }
      }
      else Seq()
    }
  }
  lazy val ATP_ARGS : Map[String, String] = {
    val a = valueOf("x")
    if(a.nonEmpty) {
      val atps = a.get
      atps.filter(_.contains("=")).map{(s : String)  =>
        val eses = s.split("=",2)
        (eses(0), eses(1))
      }.toMap.withDefault(_ => "")
    } else {
      val b = valueOf("atp-args")
      if(b.nonEmpty) {
        val atps = b.get
        atps.filter(_.contains("=")).map{(s : String)  =>
          val eses = s.split("=",2)
          (eses(0), eses(1))
        }
      }.toMap.withDefault(_ => "")
      else Map.empty.withDefault(_ => "")
    }
  }


  lazy val ATP_TIMEOUT : Map[String, Int] = {
    val a = valueOf("e")
    if(a.nonEmpty) {
      val atps = a.get
      atps.filter(_.contains("=")).map{(s : String)  =>
        val eses = s.split("=",2)
        (eses(0), eses(1).toInt)
      }.toMap.withDefault(_ => DEFAULT_ATP_TIMEOUT)
    } else {
      val b = valueOf("atp-timeout")
      if(b.nonEmpty) {
        val atps = b.get
        atps.filter(_.contains("=")).map{(s : String)  =>
          val eses = s.split("=",2)
          (eses(0), eses(1).toInt)
        }.toMap.withDefault(_ => DEFAULT_ATP_TIMEOUT)
      }
      else Map().withDefault(_ => DEFAULT_ATP_TIMEOUT)
    }
  }

  lazy val CONCURRENT_TRANSLATE : Boolean = isSet(PARAM_CONCURRENT_TRANSLATE)


  final val CAPS: String =
    """
      |{
      |  "name"         : "Leo-III",
      |  "version"      : "1.1",
      |  "applications" : ["prover"],
      |  "languages"    : [
      |    {
      |      "language" : "tptp_cnf",
      |      "roles"    : ["axiom", "conjecture", "negated_conjecture"],
      |      "features" : []
      |    },
      |    {
      |      "language" : "tptp_fof",
      |      "roles"    : ["axiom", "definition", "negated_conjecture", "conjecture"],
      |      "features" : []
      |    },
      |    {
      |      "language" : "tptp_tff",
      |      "roles"    : ["axiom", "definition", "conjecture", "negated_conjecture", "type"],
      |      "features" : ["polytypes", "conditional"]
      |    },
      |    {
      |      "language" : "tptp_thf",
      |      "roles"    : ["axiom", "definition", "conjecture", "negated_conjecture", "type"],
      |      "features" : [
      |        "polytypes", "conditional", "let",
      |        "definite_description", "indefinite_description", "th1_combinators"
      |      ]
      |    }
      |  ]
      |}
    """.stripMargin

  // more to come ...

  ///////////////
  // Help output
  ///////////////
  lazy val helptext: String = {
    val sb = StringBuilder.newBuilder
    sb.append("Leo III -- A Higher-Order Theorem Prover.\n")
    sb.append("Christoph Benzmüller, Alexander Steen, Max Wisniewski and others.\n\n")
    sb.append("Usage: ... PROBLEM_FILE [OPTIONS]\n")
    sb.append("Options:\n")
    val it = optionsMap.iterator
    while (it.hasNext) {
      val entry = it.next()
      sb.append(s"-${entry._1}")
      if (!entry._2._2.isEmpty) {
        sb.append(s" ${entry._2._2}")
      }
      if (!entry._2._1.isEmpty) {
        sb.append(s", --${entry._2._1} ${entry._2._2}")
      }
      sb.append(s"\t\t${entry._2._3}\n")
    }
    sb.append("\n")
    sb.toString
  }
  def help(): Unit = Out.output(helptext)

  ////////////
  // Utility
  ////////////
  protected def processLevel(actual: String): Level = safeStrToInt(actual) match {
    case None => DEFAULT_VERBOSITY
    case Some(0) => Level.OFF
    case Some(1) => Level.WARNING
    case Some(2) => Level.INFO
    case Some(3) => Level.CONFIG
    case Some(4) => Level.FINE
    case Some(5) => Level.FINER
    case Some(6) => Level.FINEST
    case _ =>
      Out.warn(s"Allowed verbosity levels for parameter $PARAM_VERBOSITY are integers from 0 (including) to 6 (including).")
      DEFAULT_VERBOSITY
  }

  protected def uniqueIntFor(param: String, default: Int): Int = if (configMap == null) default
    else configMap.get(param) match {
    case None => default
    case Some(arg :: Nil) => processIntFor(param, arg, default)
    case Some(arg :: _) =>
      Out.warn(multiDefOutput(param))
      processIntFor(param, arg, default)
    case Some(_) => Out.warn(intExpectedOutput(param, "None"))
      default
  }
  protected def uniqueDoubleFor(param: String, default: Double): Double = if (configMap == null) default
  else configMap.get(param) match {
    case None => default
    case Some(arg :: Nil) => processDoubleFor(param, arg, default)
    case Some(arg :: _) =>
      Out.warn(multiDefOutput(param))
      processDoubleFor(param, arg, default)
    case Some(_) => Out.warn(intExpectedOutput(param, "None"))
      default
  }
  protected def processIntFor(param: String, actual: String, default: Int): Int = {
    safeStrToInt(actual).getOrElse({
      Out.warn(intExpectedOutput(param, actual))
      default})
  }
  protected def processDoubleFor(param: String, actual: String, default: Double): Double = {
    safeStrToDouble(actual).getOrElse({
      Out.warn(intExpectedOutput(param, actual))
      default})
  }

  protected def multiDefOutput(paramName: String): Output = new Output {
    val apply = s"Parameter $paramName was defined multiple times. First occurrence is used, the rest is ignored."
  }
  protected def intExpectedOutput(paramName: String, actual: String): Output = new Output {
    val apply = s"Parameter $paramName expects an Integer value, but '$actual' was given. Default value is used."
  }
  protected def safeStrToInt(str: String): Option[Int] = try {
    Some(str.toInt)
  } catch {
    case _:Throwable => None
  }
  protected def safeStrToDouble(str: String): Option[Double] = try {
    Some(str.toDouble)
  } catch {
    case _:Throwable => None
  }

  //////////////////////////////
  // General purpose accessors
  //////////////////////////////

  def isSet(param: String): Boolean = configMap.get(param).isDefined
  def valueOf(param: String): Option[Seq[String]] = configMap.get(param) match {
    case None => None
    case Some(Seq()) => None
    case rest => rest
  }
  def isSetTo(param: String, arg: String): Boolean =
    configMap.get(param).fold(false)(args => args.length == 1 && args.head == arg)

}

trait DefaultConfiguration {
  val DEFAULT_THREADCOUNT = 4
  val DEFAULT_VERBOSITY = java.util.logging.Level.CONFIG
  val DEFAULT_TIMEOUT = 60
  val DEFAULT_SOS = false
  val DEFAULT_BOOLEXT = true
  val DEFAULT_UNIFICATIONDEPTH = 8
  val DEFAULT_MATCHINGDEPTH = 4
  val DEFAULT_UNIFIERCOUNT = 1
  val DEFAULT_PRIMSUBST = 1
  val DEFAULT_RESTRICTUNIATTEMPTS = true
  val DEFAULT_PRE_PRIMSUBST = -1
  val DEFAULT_PRE_PRIMSUBST_MAXDEPTH = 5
  val DEFAULT_ATPCALLINTERVAL = 15
  val DEFAULT_ATP_TIMEOUT = 15
  val DEFAULT_ATPMAXJOBS = 1
  val DEFAULT_PASSMARK = 0.56
  val DEFAULT_AGING = 2.35
  val DEFAULT_SUBSUMPTIONMETHOD = leo.modules.calculus.HOPatternSubsumption
  val DEFAULT_CHOICE = true
  val DEFAULT_TERMORDERING = leo.datastructures.impl.orderings.TO_CPO_Naive
  val DEFAULT_PAR_SCHED = 3
  val DEFAULT_RENAMING = true
  val DEFAULT_FUNCSPEC = false
  val DEFAULT_DOMCONSTR = 0
}
