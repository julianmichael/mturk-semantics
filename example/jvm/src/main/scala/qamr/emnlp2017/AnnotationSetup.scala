package qamr.emnlp2017

import qamr._
import qamr.annotation._
import qamr.util._

import turkey._
import turkey.tasks._

import nlpdata.structure._
import nlpdata.datasets.ptb._
import nlpdata.datasets.wiki1k._
import nlpdata.datasets.wiktionary.Inflections
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text

import akka.actor._
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source

import scala.concurrent.duration._
import scala.language.postfixOps

import scala.util.Random
import scala.util.Try

import monocle._
import monocle.macros._

import upickle.default._

/** Replicates the annotation setup for our emnlp2017 submission. */
class AnnotationSetup(implicit config: TaskConfig) {

  import config._

  import java.nio.file.{Paths, Path, Files}
  private[this] val dataPath = Paths.get("live-data")
  implicit val annotationDataService = new AnnotationDataService {
    override def saveLiveData(name: String, contents: String): Try[Unit] = Try {
      val directory = dataPath
      if(!Files.exists(directory)) {
        Files.createDirectories(directory)
      }
      val path = directory.resolve(name)
      Files.write(path, contents.getBytes())
    }

    override def loadLiveData(name: String): Try[List[String]] = Try {
      val directory = dataPath
      if(!Files.exists(directory)) {
        Files.createDirectories(directory)
      }
      val path = directory.resolve(name)
      import scala.collection.JavaConverters._
      Files.lines(path).iterator.asScala.toList
    }
  }

  lazy val origQASRLPaths = read[Vector[PTBSentencePath]](
    loadDataFile("origQASRLPaths.txt").get.head
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

  implicit lazy val isStopword = IsStopword(isReallyUninteresting)

  def numGenerationAssignmentsForPrompt(p: GenerationPrompt[SentenceId]) = p.id match {
    case PTBSentenceId(_) => 5
    case id @ WikiSentenceId(_) => if(isTrain(id)) 1 else 3
  }

  lazy val experiment = new AnnotationPipeline(allIds, numGenerationAssignmentsForPrompt)
}
