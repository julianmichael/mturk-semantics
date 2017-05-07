package mts.util

trait PackagePlatformExtensions {
  def sendToClipboard(s: String): Unit = {
    import java.awt._;
    import java.awt.datatransfer._;
    import java.io._;
    val selection = new StringSelection(s)
    val clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    clipboard.setContents(selection, selection)
  }

  // analysis

  import mts.util._
  import nlpdata.datasets.conll._

  // the pred itself, discourse markers, negations, and auxiliaries we don't care about
  def labelIsIrrelevant(l: String) = {
    l == "V" || l.contains("DIS") || l.contains("NEG") || l.contains("MOD") ||
      l.contains("C-") || l.contains("R-") ||
      l == "rel"// || l == "Support"
  }

  case class PrecisionRecall(
    numPredicted: Double,
    numGold: Double,
    numCorrect: Double,
    numCovered: Double) {
    val precision = numCorrect / numPredicted
    val recall = numCovered / numGold
    val f1 = 2 * precision * recall / (precision + recall)

    def statString = f"F1: $f1%.3f\tPrecision: $precision%.3f\tRecall: $recall%.3f"

    def aggregate(other: PrecisionRecall) = PrecisionRecall(
      numPredicted + other.numPredicted,
      numGold + other.numGold,
      numCorrect + other.numCorrect,
      numCovered + other.numCovered)
  }
  object PrecisionRecall {
    val zero = PrecisionRecall(0, 0, 0, 0)
  }

  def statString(pr: PrecisionRecall) = pr.statString

  def histogramString(hist: Scorer[Int, Int]): String = {
    val vec = (0 to hist.keyIterator.max).map(hist.get).toVector
    val max = vec.max
    val scaleMax = 50.0
    val scaleFactor = scaleMax / max
    def scale(n: Int): Int = math.ceil(n.toDouble * scaleFactor).toInt
    def pounds(n: Int) = "#" * n
    vec.zipWithIndex
      .map { case (n, i) => f"$i%3d|${pounds(scale(n))}%s $n%d"}
      .mkString("\n")
  }

  // natural language stuff

  import edu.stanford.nlp.ling.Word
  import edu.stanford.nlp.process.PTBTokenizer
  import edu.stanford.nlp.process.WordTokenFactory

  import java.io.StringReader
  import java.nio.file.{Paths, Path, Files}

  import nlpdata.util.LowerCaseStrings._

  private[this] val stopwordFilePath = Paths.get("english.stop.txt")
  private[this] val conservativeStopwordFilePath = Paths.get("english-stop-conservative.txt")

  /** Stopword set from a local file.
    *
    * Not sure where the file came from, but I found it in my old repo
    * from Dan Garrette's undergrad NLP class.
    */
  lazy val stopwords: Set[LowerCaseString] = {
    val setResource = for {
      lines <- FileManager.loadResource(stopwordFilePath)
    } yield (lines.toSet ++ Set("hm", "uh", "um")).map(_.lowerCase)
    setResource.tried.get
  }

  // I deleted some stopwords that we actually want
  lazy val conservativeStopwords: Set[LowerCaseString] = {
    val setResource = for {
      lines <- FileManager.loadResource(conservativeStopwordFilePath)
    } yield (lines.toSet ++ Set("hm", "uh", "um")).map(_.lowerCase)
    setResource.tried.get
  }

  /** (non-normalized as well as normalized PTB tokens.) */
  val punctuation = Set[String](
    ".", ",", "!", "?", ";", ":", "...",
    "''", "\"", "'", "`", "``",
    "#", "--", "-", "–", "—", "%", // PTB dashes, hyphens, en and em dashes
    "−", // minus sign (unicode hex 2122)
    "+", "±", "<", "≤", "≥", ">", "=",
    "^", "@", "|", "&",
    "/.", "/?", "/", "\\",
    ")", "]", "}",
    "(", "[", "{",
    "-RRB-", "-RCB-", "-RSB-",
    "-LRB-", "-LCB-", "-LSB-")

  val contractions = Set("n't", "'s", "'re", "'ve", "'ll", "na", "'m", "'d")

  val questionWords = Set("who", "what", "when", "where", "why", "how",
                          "whose", "which", "much", "many")

  // NOTE: can get java code with pronouns from HITL stuff
  // for now, settle with this

  val pronouns = Set(
    "I", "me", "my", "mine",
    "we", "us", "our", "ours",
    "you", "your", "yours",
    "he", "him", "his",
    "she", "her", "hers",
    "it", "its",
    "they", "them", "their",
    "someone", "something",
    "this", "that"
  ).map(_.lowerCase)

  lazy val uninterestingTokens = stopwords ++ punctuation ++ contractions
  lazy val reallyUninterestingTokens = conservativeStopwords ++ punctuation ++ contractions ++ questionWords

  def isReallyUninteresting(t: String) = reallyUninterestingTokens.contains(t) ||
    reallyUninterestingTokens.contains(t.toLowerCase)

  /** Tokenizes an English string. */
  def tokenize(s: String): List[String] = {
    import scala.collection.JavaConverters._
    new PTBTokenizer(new StringReader(s), new WordTokenFactory(), "")
      .tokenize.asScala.toList.map(_.word)
  }

  // pos-tagging

  import edu.stanford.nlp.tagger.maxent.MaxentTagger
  lazy val tagger: MaxentTagger  = new MaxentTagger("resources/corenlp/stanford-postagger-2016-10-31/models/english-left3words-distsim.tagger");

  case class POSTaggedToken(token: String, pos: String)

  /** POS-tags a sequence of tokens. */
  def posTag(s: List[String]): List[POSTaggedToken] = {
    tagger.tagTokenizedString(s.mkString(" ")).split(" ").toList
      .map(_.split("_"))
      .map(s => POSTaggedToken(s(0), s(1)))
  }
}
