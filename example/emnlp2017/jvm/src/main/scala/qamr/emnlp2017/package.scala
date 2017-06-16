package qamr

import qamr.annotation._

import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory

import java.io.StringReader
import java.nio.file.{Paths, Path, Files}

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text
import nlpdata.datasets.ptb
import nlpdata.datasets.propbank
import nlpdata.datasets.nombank
import nlpdata.datasets.qasrl
import nlpdata.datasets.wiki1k
import nlpdata.datasets.wiktionary
import nlpdata.datasets.wiktionary.Inflections

import scala.util.Try

package object emnlp2017 {

  import java.nio.file.{Paths, Path, Files}
  private[this] val liveDataPath = Paths.get("live-data/emnlp2017")
  val liveAnnotationDataService = new FileSystemAnnotationDataService(liveDataPath)

  val staticDataPath = Paths.get("static-data/emnlp2017")

  def saveDataFile(name: String, contents: String): Try[Unit] = Try {
    val directory = staticDataPath.resolve("out")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    Files.write(path, contents.getBytes())
  }

  def loadDataFile(name: String): Try[List[String]] = Try {
    val directory = staticDataPath.resolve("in")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  def getWordsInQuestion(
    sentence: Vector[String],
    questionTokens: Vector[String])(
    implicit inflections: Inflections,
    isStopword: IsStopword
  ): Set[Int] = {
    val tokens = questionTokens.filterNot(isStopword)
    val moreTokens = tokens.map(t => Text.normalizeToken(t).lowerCase).flatMap(inflections.getAllForms)
    val generalizedTokens = tokens.map(_.lowerCase) ++ moreTokens
    sentence.zipWithIndex.filter(p => generalizedTokens.contains(p._1.lowerCase)).map(_._2).toSet
  }

  def getAlignedQuestionIndices(
    sentence: Vector[String],
    questionTokens: Vector[String])(
    implicit inflections: Inflections,
    isStopword: IsStopword
  ): Set[Int] = {
    val lowerSentence = sentence.map(_.lowerCase)
    val allIndices = for {
      (t, index) <- questionTokens.zipWithIndex
      if !isStopword(t)
      lowerToken = Text.normalizeToken(t).lowerCase
      tokenForm <- t.lowerCase :: inflections.getAllForms(lowerToken).toList
      if lowerSentence.contains(tokenForm)
    } yield index
    allIndices.toSet
  }

  def getQuestionSentenceAlignments(
    sentence: Vector[String],
    questionTokens: Vector[String])(
    implicit inflections: Inflections,
    isStopword: IsStopword
  ): Set[(Int, Int)] = {
    val lowerSentence = sentence.map(_.lowerCase)
    val lowerQuestion = questionTokens.map(_.lowerCase)
    val allIndices = for {
      (qToken, qIndex) <- lowerQuestion.zipWithIndex
      if !isStopword(qToken)
      lowerQToken = Text.normalizeToken(qToken).lowerCase
      qTokenForm <- qToken :: inflections.getAllForms(lowerQToken).toList
      (sToken, sIndex) <- lowerSentence.zipWithIndex
      lowerSToken = Text.normalizeToken(sToken).lowerCase
      sTokenForm <- sToken :: inflections.getAllForms(lowerSToken).toList
      if qTokenForm.equals(sTokenForm)
    } yield (qIndex, sIndex)
    allIndices.toSet
  }

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

  private[this] val conservativeStopwordFilePath = resourcePath.resolve("english-stop-conservative.txt")

  /** Stopword set from a local file.
    *
    * Not sure where the file came from, but I found it in my old repo
    * from Dan Garrette's undergrad NLP class.
    * I deleted some stopwords that we actually want.
    */
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
