package turksem.qasrl.multitask

import cats._
import cats.implicits._

import turksem.FileSystemAnnotationDataService
import turksem.qamr.annotation._
import turksem.qasrl.annotation._

import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory
import java.io.StringReader
import java.nio.file.{Files, Path, Paths}

import nlpdata.util.LowerCaseStrings._
import nlpdata.util._
import nlpdata.datasets.ptb3._
import nlpdata.datasets.qasrl
import nlpdata.datasets.wiktionary
import nlpdata.datasets.wiktionary.Inflections
import turksem.IsStopword

import scala.util.Try
import scala.util.Random
import upickle.default._

trait PackagePlatformExtensions {

  // TODO define these if upickle serialization isn't sufficient

  // def sentenceIdToString(sid: SentenceId): String = sid match {
  //   case WSJPath(section, number) => f"WSJ:$section%02d$number%02d"
  //   case BrownPath(domain, number) => f"Brown:$domain%s$number%02d"
  // }

  // val WSJMatchRegex = "WSJ:([0-9]{2})([0-9]{2})".r
  // val BrownMatchRegex = "Brown:([^:]+):([^:]+):([0-9]+):([0-9]+)".r

  // protected[nlpdata] object IntMatch {
  //   val IntMatchRegex = "(\\d+)".r
  //   def unapply(s: String): Option[Int] = s match {
  //     case IntMatchRegex(num) => Some(num.toInt)
  //     case _ => None
  //   }
  // }

  // def sentenceIdFromString(s: String): SentenceId = s match {
  //   case PTBMatch(suffix, sentenceNum) =>
  //     PTBSentenceId(PTBSentencePath(PTBPath(suffix), sentenceNum.toInt))
  //   case Wiki1kMatch(domain, suffix, paragraphNum, sentenceNum) =>
  //     WikiSentenceId(Wiki1kSentencePath(Wiki1kPath(domain, suffix), paragraphNum.toInt, sentenceNum.toInt))
  // }

  implicit object SentenceIdHasTokens extends HasTokens[SentenceId] {
    def getTokens(id: SentenceId): Vector[String] = PTB.getSentence(id).tokens
  }

  import java.nio.file.{Paths, Path, Files}
  private[this] val liveDataPath = Paths.get("live-data/multitask")
  val liveAnnotationDataService = new FileSystemAnnotationDataService(liveDataPath)

  val staticDataPath = Paths.get("static-data/multitask")

  def saveOutputFile(name: String, contents: String): Try[Unit] = Try {
    val directory = staticDataPath.resolve("out")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    Files.write(path, contents.getBytes())
  }

  def loadOutputFile(name: String): Try[List[String]] = Try {
    val directory = staticDataPath.resolve("out")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  def loadInputFile(name: String): Try[List[String]] = Try {
    val directory = staticDataPath.resolve("in")
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    val path = directory.resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  val resourcePath = java.nio.file.Paths.get("resources")

  // ignore file system errors.. the service should always succeed
  val PTB = {
    val getTry = new (Try ~> Id) {
      def apply[A](a: Try[A]): Id[A] = a.get
    }
    new InterpretedPTB3Service(
      getTry compose (new PTB3FileSystemInterpreter(resourcePath.resolve("ptb3")))
    )
  }

  val Wiktionary = new wiktionary.WiktionaryFileSystemService(
    resourcePath.resolve("wiktionary")
  )

  // val QASRL = new qasrl.QASRLFileSystemService(
  //   resourcePath.resolve("qasrl"), PTB
  // )

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

  val posTagCache = collection.mutable.Map.empty[Vector[String], Vector[POSTaggedToken]]

  import cats.Foldable
  import cats.implicits._
  /** POS-tags a sequence of tokens. */
  def posTag[F[_]: Foldable](s: F[String]): Vector[POSTaggedToken] = {
    val origTokens = s.toList.toVector // to prevent americanization.
    // probably we can do that with a tagger parameter...but...how about later..
    posTagCache.get(origTokens) match {
      case None =>
        val result = tagger.tagTokenizedString(origTokens.mkString(" ")).split(" ").toVector
          .map(_.split("_"))
          .zipWithIndex
          .map { case (s, index) => POSTaggedToken(origTokens(index), s(1)) }
        posTagCache.put(origTokens, result)
        result
      case Some(result) => result
    }
  }

  lazy val allIds = {
    val allPaths = PTB.getAllPaths
    val eligibleBrownPaths = allPaths.collect {
      case p @ BrownPath("CK", number) if number > 3 => p
    }
    val eligibleSentencePaths = for {
      path <- eligibleBrownPaths
      sentence <-  PTB.getFile(path).sentences
    } yield sentence.path
    eligibleSentencePaths.toVector
  }

  implicit lazy val inflections = {
    val tokens = for {
      id <- allIds.iterator
      word <- id.tokens.iterator
    } yield word
    Wiktionary.getInflectionsForTokens(tokens)
  }

  implicit val isStopword = IsStopword(isReallyUninteresting)
}
