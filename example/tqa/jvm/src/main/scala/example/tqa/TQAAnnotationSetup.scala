package example.tqa

import cats._
import cats.implicits._

import turksem.FileSystemAnnotationDataService
import turksem.qasrl._
import turksem.util._

import spacro._
import spacro.tasks._

import nlpdata.structure.AlignedToken

import nlpdata.datasets.wiktionary
import nlpdata.datasets.wiktionary.Inflections
import nlpdata.datasets.tqa.TQAFileSystemService

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text
import nlpdata.util.HasAlignedTokens
import nlpdata.util.HasAlignedTokens.ops._

import akka.actor._
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source

import scala.concurrent.duration._
import scala.language.postfixOps

import scala.util.Try

import upickle.default._

import java.io.StringReader
import java.nio.file.{Files, Path, Paths}

import scala.util.Try
import scala.util.Random

import upickle.default._

class TQAAnnotationSetup(
  val isGold: Boolean,
  val label: String,
  frozenGenerationHITTypeId: Option[String] = None,
  frozenValidationHITTypeId: Option[String] = None)(
  implicit config: TaskConfig) {

  val resourcePath = java.nio.file.Paths.get("resources")

  import java.nio.file.{Paths, Path, Files}
  private[this] val liveDataPath = Paths.get(s"data/tqa/$label/live")
  val liveAnnotationDataService = new FileSystemAnnotationDataService(liveDataPath)

  val staticDataPath = Paths.get(s"data/tqa/$label/static")

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

  lazy val tqaTexts = new TQAFileSystemTopicTextService(
    resourcePath.resolve("tqa/tqa_sentences_trimmed.json")
  ).topicTexts

  lazy val tqaTrain = new TQAFileSystemService(
    resourcePath.resolve("tqa/train/tqa_v1_train.json")
  ).getDataset.get
  lazy val tqaDev = new TQAFileSystemService(
    resourcePath.resolve("tqa/val/tqa_v1_val.json")
  ).getDataset.get

  lazy val tqaTrainTopicIds = tqaTrain.topics.keySet
  lazy val tqaDevTopicIds = tqaDev.topics.keySet

  def isTQATrain(sid: TQASentenceId) = tqaTrainTopicIds.contains(sid.topicId)
  def isTQADev(sid: TQASentenceId) = tqaDevTopicIds.contains(sid.topicId)
  def isTQATest(sid: TQASentenceId) = !isTQATrain(sid) && !isTQADev(sid)

  lazy val Wiktionary = new wiktionary.WiktionaryFileSystemService(
    resourcePath.resolve("wiktionary")
  )

  lazy val tusharSentences = loadInputFile("qasrl_sentences_only.csv").get.toVector.map(AligningTokenizer.tokenize)

  lazy val tusharIds = tusharSentences.indices.map(TusharSentenceId(_)).toVector

  implicit object SentenceIdHasAlignedTokens extends HasAlignedTokens[SentenceId] {
    def getAlignedTokens(id: SentenceId): Vector[AlignedToken] = id match {
      case TQASentenceId(globalId, sentenceIndex) => tqaTexts(globalId).sentences(sentenceIndex)
      case TusharSentenceId(index) => tusharSentences(index)
    }
  }

  import scala.annotation.tailrec
  @tailrec private def weightedRoundRobinAux[A](soFar: Vector[A], vectors: List[Vector[A]]): Vector[A] = {
      if(vectors.isEmpty) soFar else { // hit base case because filter out empties
        val smallestSize = vectors.map(_.size).min // works bc nonempty
        val (processedRemains, newSoFar) = vectors.foldLeft((List.empty[Vector[A]], soFar)) {
          case ((remains, fullSoFar), vector) =>
            val sizeMultiplier = vector.size / smallestSize
            (vector.drop(sizeMultiplier) :: remains, fullSoFar ++ vector.take(sizeMultiplier))
        }
        weightedRoundRobinAux(newSoFar, processedRemains.reverse.filter(_.nonEmpty))
      }
    }
  def weightedRoundRobin[A](vectors: List[Vector[A]]) = weightedRoundRobinAux(Vector.empty[A], vectors)

  // reorders a vector into a result s.t. prefixes of the result are
  // (roughly) maximally evenly distributed over the original vector
  private def evenDistributionAux[A](vector: Vector[A]): Iterator[A] = {
    if(vector.size <= 3) {
      vector.iterator
    } else {
      val (firstHalf, secondHalf) = vector.splitAt(vector.size / 2)
      evenDistributionAux(firstHalf).zip(evenDistributionAux(secondHalf)).flatMap {
        case (x, y) => x :: y :: Nil
      }
    }
  }
  def evenDistribution[A](vector: Vector[A]) = evenDistributionAux(vector).toVector

  lazy val (tqaTrainIds, tqaDevIds, tqaTestIds) = {
    val unshuffled = for {
      (topicId, topicText) <- tqaTexts
      index <- topicText.sentences.indices
    } yield TQASentenceId(topicId, index)
    val shuffleRand = new Random(2158369L)
    val shuffled = shuffleRand.shuffle(unshuffled.toVector)
    val train = shuffled.filter(isTQATrain)
    val dev = shuffled.filter(isTQADev)
    val test = shuffled.filter(isTQATest)
    (train, dev, test)
  }

  lazy val allIds = weightedRoundRobin(List(tqaTrainIds, tqaDevIds, tqaTestIds, tusharIds))

  lazy val goldIds = evenDistribution(tqaDevIds)

  lazy val expIds = if(isGold) goldIds else allIds

  implicit lazy val inflections = {
    val tokens = for {
      id <- allIds.iterator
      word <- id.tokens.iterator
    } yield word
    Wiktionary.getInflectionsForTokens(tokens)
  }

  def numGenerationAssignmentsForPrompt(p: QASRLGenerationPrompt[SentenceId]) = 1

  lazy val experiment = new QASRLAnnotationPipeline(
    expIds, numGenerationAssignmentsForPrompt,
    liveAnnotationDataService,
    frozenGenerationHITTypeId = frozenGenerationHITTypeId,
    frozenValidationHITTypeId = frozenValidationHITTypeId,
    generationAccuracyDisqualTypeLabel = None,
    generationCoverageDisqualTypeLabel = None,
    validationAgreementDisqualTypeLabel = None)

  def saveAnnotationData(
    filename: String,
    ids: Vector[SentenceId]
  ) = {
    saveOutputFile(
      s"$filename-readable.tsv",
      DataIO.makeReadableQAPairTSV(
        ids.toList,
        SentenceId.toString,
        identity,
        experiment.allGenInfos,
        experiment.allValInfos,
        (id: SentenceId, qa: VerbQA, responses: List[QASRLValidationAnswer]) => responses.forall(_.isAnswer))
    )
    saveOutputFile(
      s"$filename.tsv",
      DataIO.makeQAPairTSV(
        ids.toList,
        SentenceId.toString,
        experiment.allGenInfos,
        experiment.allValInfos)
    )
  }

  def writeAllTSVs = {
    () // TODO
  }
}
