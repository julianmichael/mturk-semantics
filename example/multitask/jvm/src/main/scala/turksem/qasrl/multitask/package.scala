package turksem.qasrl

import turksem.FileSystemAnnotationDataService
import turksem.qamr.annotation._
import turksem.qasrl.annotation._

import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory
import java.io.StringReader
import java.nio.file.{Files, Path, Paths}

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text
import nlpdata.datasets.ptb
import nlpdata.datasets.ptb.PTBSentencePath
import nlpdata.datasets.propbank
import nlpdata.datasets.nombank
import nlpdata.datasets.qasrl
import nlpdata.datasets.wiki1k
import nlpdata.datasets.wiki1k.Wiki1kPath
import nlpdata.datasets.wiktionary
import nlpdata.datasets.wiktionary.Inflections
import turksem.{HasTokens, IsStopword}

import scala.util.Try
import scala.util.Random
import upickle.default._

package object multitask {

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

  case class InflectedAlignment(
    index: Int,
    reinflectionOpt: Option[Int])

  def getReinflectedQuestionSentenceAlignments(
    sentence: Vector[String],
    questionTokens: Vector[String])(
    implicit inflections: Inflections,
    isStopword: IsStopword
  ): Map[Int, List[InflectedAlignment]] = {
    val lowerSentence = sentence.map(_.lowerCase)
    val lowerQuestion = questionTokens.map(_.lowerCase)
    val allIndices = for {
      (sToken, sIndex) <- lowerSentence.zipWithIndex
      if !isStopword(sToken)
      lowerSToken = Text.normalizeToken(sToken).lowerCase
      inflectedFormsOpt = inflections.getInflectedForms(lowerSToken)
      allForms = inflections.getAllForms(sToken)
      (lowerQToken, qIndex) <- lowerQuestion.zipWithIndex
      if !isStopword(lowerQToken)
      res <- {
        if(lowerSToken == lowerQToken) {
          List(qIndex -> InflectedAlignment(sIndex, None)) // no reinflection necessary
        } else if(!allForms.contains(lowerQToken)) {
          Nil // no alignment
        } else {
          List(qIndex -> InflectedAlignment(sIndex, inflectedFormsOpt.flatMap(_.indexOpt(lowerQToken))))
        }
      }
    } yield res
    allIndices.groupBy(_._1).map {
      case (qi, pairs) => qi -> pairs.map(_._2).toList
    }
  }

  val ptbVerbPosTags = Set("VB", "VBD", "VBG", "VBN", "VBP", "VBZ")
  val ptbNounPosTags = Set("NN", "NNS", "NNP", "NNPS")

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

  // data for experiment

  lazy val origQASRLPaths = read[Vector[PTBSentencePath]](
    loadInputFile("origQASRLPaths.txt").get.head
  )

  val numPTB = 150

  lazy val (ptbTrain, ptbDev, ptbTest) = {
    val shuffleRand = new Random(832592735L)
    val (train, devTestRest) = shuffleRand.shuffle(origQASRLPaths).splitAt(numPTB * 4 / 5)
    val (dev, testRest) = devTestRest.splitAt(numPTB / 10)
    val test = testRest.take(numPTB / 10)
    (train, dev, test)
  }

  lazy val ptb100ForAMR = PTB.allPTBSentencePaths.get.take(103).map(PTBSentenceId.apply).toList

  def getWikiSentences(rand: Random, filePaths: Vector[Wiki1kPath], numSentences: Int) = {
    rand.shuffle(
      filePaths.flatMap(p => Wiki1k.getFile(p).get.paragraphs)
    ).filter(p =>
      !p.exists(sentence =>
        sentence.tokens.exists(t =>
          Text.normalizeToken(t) == "\\"))
    ).flatten.map(s => s.path).take(numSentences)
  }

  val numWikipedia = 2500
  // val numWikipedia = 5

  lazy val (wikipediaTrain, wikipediaDev, wikipediaTest) = {
    val shuffleRand = new Random(1230976L)
    val (trainFiles, devTestRestFiles) = shuffleRand.shuffle(
      Wiki1k.wiki1kPathsForDomain("wikipedia")
    ).splitAt(640)
    val (devFiles, testRestFiles) = devTestRestFiles.splitAt(80)
    val testFiles = testRestFiles.take(80)

    val train = getWikiSentences(shuffleRand, trainFiles, numWikipedia * 4 / 5)
    val dev = getWikiSentences(shuffleRand, devFiles, numWikipedia / 10)
    val test = getWikiSentences(shuffleRand, testFiles, numWikipedia / 10)
    (train, dev, test)
  }

  val numWikinews = 2500
  // val numWikinews = 5

  lazy val (wikinewsTrain, wikinewsDev, wikinewsTest) = {
    val shuffleRand = new Random(1246902L)
    val (trainFiles, devTestRestFiles) = shuffleRand.shuffle(
      Wiki1k.wiki1kPathsForDomain("wikinews")
        .sortBy(-_.suffix.toInt) // relies on wikinews IDs being ints... true as of now
        .take(1000)
    ).splitAt(800)
    val (devFiles, testRestFiles) = devTestRestFiles.splitAt(80)
    val testFiles = testRestFiles.take(80)

    val train = getWikiSentences(shuffleRand, trainFiles, numWikinews * 4 / 5)
    val dev = getWikiSentences(shuffleRand, devFiles, numWikinews / 10)
    val test = getWikiSentences(shuffleRand, testFiles, numWikinews / 10)
    (train, dev, test)
  }

  lazy val trainIds = ptbTrain.map(PTBSentenceId(_): SentenceId) ++
    wikipediaTrain.map(WikiSentenceId(_): SentenceId) ++
    wikinewsTrain.map(WikiSentenceId(_): SentenceId)
  lazy val trainIDSet = trainIds.toSet
  def isTrain(id: SentenceId) = trainIDSet.contains(id)

  lazy val devIds = ptbDev.map(PTBSentenceId(_): SentenceId) ++
    wikipediaDev.map(WikiSentenceId(_): SentenceId) ++
    wikinewsDev.map(WikiSentenceId(_): SentenceId)
  lazy val devIDSet = devIds.toSet
  def isDev(id: SentenceId) = devIDSet.contains(id)

  lazy val testIds = ptbTest.map(PTBSentenceId(_): SentenceId) ++
    wikipediaTest.map(WikiSentenceId(_): SentenceId) ++
    wikinewsTest.map(WikiSentenceId(_): SentenceId)
  lazy val testIDSet = testIds.toSet
  def isTest(id: SentenceId) = testIDSet.contains(id)

  lazy val sourceIds = {
    val idShuffleRand = new Random(218469L)
    idShuffleRand.shuffle(trainIds ++ devIds ++ testIds)
      .filter {
      case WikiSentenceId(path) =>
        !path.filePath.suffix.contains("785582") && // this is apparently a FRENCH INTERVIEW
          !path.filePath.suffix.contains("648587") // this is apparently a SPANISH ARTICLE
      case _ => true
    }
  }

  lazy val allIds = (ptb100ForAMR ++ sourceIds).toVector

  implicit lazy val inflections = {
    val tokens = for {
      id <- allIds.iterator
      word <- id.tokens.iterator
    } yield word
    Wiktionary.getInflectionsForTokens(tokens)
  }

  implicit val isStopword = IsStopword(isReallyUninteresting)

  implicit class RichIterable[A](val a: Iterable[A]) extends AnyVal {
    def minOption(implicit o: Ordering[A]): Option[A] = if(a.isEmpty) None else Some(a.min)
    def maxOption(implicit o: Ordering[A]): Option[A] = if(a.isEmpty) None else Some(a.max)
  }

  implicit class RichSeq[A](val as: Seq[A]) extends AnyVal {
    def indexOpt(a: A): Option[Int] = Some(as.indexOf(a)).filter(_ >= 0)
    def indexFind(p: A => Boolean) = as.zipWithIndex.find(pair => p(pair._1)).map(_._2)
    // TODO doesn't short circuit when it finds the guy
    def indicesYielding[B](f: A => Option[B]): Seq[(Int, B)] =
      as.zipWithIndex.flatMap(pair => f(pair._1).map(b => (pair._2, b)))
  }
}
