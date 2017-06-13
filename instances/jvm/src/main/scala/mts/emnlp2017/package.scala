package mts

import mts._

import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory

import java.io.StringReader
import java.nio.file.{Paths, Path, Files}

import nlpdata.util.LowerCaseStrings._
import nlpdata.datasets.ptb
import nlpdata.datasets.propbank
import nlpdata.datasets.nombank
import nlpdata.datasets.qasrl
import nlpdata.datasets.wiki1k
import nlpdata.datasets.wiktionary

package object emnlp2017 {

  val experimentName = "h_final"

  def getPTBSentenceTokens(sentence: ptb.PTBSentence): Vector[String] = {
    sentence.words.filter(_.pos != "-NONE-").map(_.token)
  }

  def getTokensForId(id: SentenceId): Vector[String] = id match {
    case PTBSentenceId(path) => getPTBSentenceTokens(
      PTB.getSentence(path).get
    )
    case WikiSentenceId(path) =>
      Wiki1k.getSentence(path).get.tokens
  }

  implicit val sentenceIdHasTokens = new HasTokens[SentenceId] {
    def getTokens(id: SentenceId): Vector[String] = getTokensForId(id)
  }


  val resourcePath = java.nio.file.Paths.get("resources")

  val PTB = new ptb.PTBFileSystemService(
    resourcePath.resolve("ptb")
  )

  val PropBank = new propbank.PropBankFileSystemService(
    resourcePath.resolve("propbank")
  )

  val NomBank = new nombank.NomBankFileSystemService(
    resourcePath.resolve("nombank.1.0"), PTB
  )

  val QASRL = new qasrl.QASRLFileSystemService(
    resourcePath.resolve("qasrl"), PTB
  )

  val Wiki1k = new wiki1k.Wiki1kFileSystemService(
    resourcePath.resolve("wiki1k")
  )

  val Wiktionary = new wiktionary.WiktionaryFileSystemService(
    resourcePath.resolve("wiktionary")
  )

  private[this] val stopwordFilePath = resourcePath.resolve("english.stop.txt")
  private[this] val conservativeStopwordFilePath = resourcePath.resolve("english-stop-conservative.txt")

  /** Stopword set from a local file.
    *
    * Not sure where the file came from, but I found it in my old repo
    * from Dan Garrette's undergrad NLP class.
    */
  lazy val stopwords: Set[LowerCaseString] = {
    import scala.collection.JavaConverters._
    val wordLines = Files.lines(stopwordFilePath).iterator.asScala.toSet
    (wordLines ++ Set("hm", "uh", "um")).map(_.lowerCase)
  }

  // I deleted some stopwords that we actually want
  lazy val conservativeStopwords: Set[LowerCaseString] = {
    import scala.collection.JavaConverters._
    val wordLines = Files.lines(conservativeStopwordFilePath).iterator.asScala.toSet
    (wordLines ++ Set("hm", "uh", "um")).map(_.lowerCase)
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
  def tokenize(s: String): Vector[String] = {
    import scala.collection.JavaConverters._
    new PTBTokenizer(new StringReader(s), new WordTokenFactory(), "")
      .tokenize.asScala.toVector.map(_.word)
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
