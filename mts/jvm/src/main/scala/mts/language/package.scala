package mts.language

import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory

import java.io.StringReader
import java.nio.file.{Paths, Path, Files}

import mts.util._
import mts.util.LowerCaseStrings._

/** Provides classes and methods for working with natural language strings.
  *
  * This includes resources like a set of stopwords (currently not being used),
  * a tokenizer (provided by Stanford CoreNLP),
  * and inflections (drawn from Wiktionary).
  */
trait PackagePlatformExtensions {

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
    ".", ",", "!", "?", ";", ":",
    "''", "\"", "'", "`", "``",
    "$", "#", "--", "-", "–", "—", "%", // PTB dashes, hyphens, en and em dashes
    "-", "+", "<", ">", "^", "@", "|", "&",
    "/.", "/?", "/", "\\",
    ")", "]", "}",
    "(", "[", "{",
    "-RRB-", "-RCB-", "-RSB-",
    "-LRB-", "-LCB-", "-LSB-")

  val contractions = Set("n't", "'s", "'re", "'ve", "'ll", "na", "'m", "'d")

  val questionWords = Set("who", "what", "when", "where", "why", "how",
                          "whose", "which", "much", "many")

  // NOTE: can get java code with pronouns from HITL stuff

  lazy val uninterestingTokens = stopwords ++ punctuation ++ contractions
  lazy val reallyUninterestingTokens = conservativeStopwords ++ punctuation ++ contractions ++ questionWords

  def isReallyUninteresting(t: String) = reallyUninterestingTokens.contains(t) ||
    reallyUninterestingTokens.contains(t.toLowerCase)

  /** Constructs an Inflections object containing all known inflections
    * for a given set of words.
    *
    * @param tokens an iterator over all words we might want inflections for
    */
  def getInflectionsForTokens(tokens: Iterator[String]): Inflections = {
    // XXX should rework this to use standard location instead of hack
    val wiktionaryFilepath = Paths.get("resources/wiktionary").resolve("en_verb_inflections.txt")
    val wordDict = new CountDictionary()
    tokens.foreach(wordDict.addString)
    val inflDict = new VerbInflectionDictionary(wordDict)
    inflDict.loadDictionaryFromFile(wiktionaryFilepath.toString)
    new Inflections(inflDict)
  }

  /** Tokenizes an English string. */
  def tokenize(s: String): List[String] = {
    import scala.collection.JavaConverters._
    new PTBTokenizer(new StringReader(s), new WordTokenFactory(), "")
      .tokenize.asScala.toList.map(_.word)
  }

}
