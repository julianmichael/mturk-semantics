package example.qacmp

import cats._
import cats.implicits._

import qasrl.crowd._
import turksem.util._

import spacro._
import spacro.tasks._

import nlpdata.structure.AlignedToken

import nlpdata.datasets.wiktionary.WiktionaryFileSystemService
import nlpdata.datasets.wiktionary.Inflections
import nlpdata.datasets.wiktionary.VerbForm

import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._

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

class QACmpAnnotationSetup(
  val label: String,
  frozenGenerationHITTypeId: Option[String] = None,
  frozenValidationHITTypeId: Option[String] = None)(
  implicit config: TaskConfig) {

  val resourcePath = java.nio.file.Paths.get("resources")

  private[this] val liveDataPath = Paths.get(s"data/tqa/$label/live")
  val liveAnnotationDataService = new FileSystemAnnotationDataService(liveDataPath)

  val staticDataPath = Paths.get(s"data/tqa/$label/static")

  def saveOutputFile(name: String, contents: String): Try[Unit] = Try {
    val path = staticDataPath.resolve("out").resolve(name)
    val directory = path.getParent
    if(!Files.exists(directory)) {
      Files.createDirectories(directory)
    }
    Files.write(path, contents.getBytes())
  }

  def loadOutputFile(name: String): Try[List[String]] = Try {
    val path = staticDataPath.resolve("out").resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  def loadInputFile(name: String): Try[List[String]] = Try {
    val path = staticDataPath.resolve("in").resolve(name)
    import scala.collection.JavaConverters._
    Files.lines(path).iterator.asScala.toList
  }

  // lazy val PTB = {
  //   val getTry = new (Try ~> Id) {
  //     def apply[A](a: Try[A]): Id[A] = a.get
  //   }
  //   new InterpretedPTB3Service(
  //     getTry compose (new PTB3FileSystemInterpreter(resourcePath.resolve("ptb3")))
  //   )
  // }

  // lazy val qasrlService = new QASRLFileSystemService(resourcePath.resolve("qasrl"), PTB)
  // lazy val QASRL = qasrlService.getQASRL.get

  def readQASRLFile(fileRemainder: List[String]): List[(SentenceId, Vector[String])] = {
    if(fileRemainder.size <= 1) Nil else {
      val (entryLabel :: sentence :: entries, _ :: restOfFile) = fileRemainder.span(_.trim.nonEmpty)
      val Array(sentenceId, numVerbs) = entryLabel.split("\t")
      val tokens = sentence.split(" ").toVector
      (SentenceId(sentenceId) -> tokens) :: readQASRLFile(restOfFile)
    }
  }

  lazy val qasrlSentences: Map[SentenceId, Vector[String]] = {
    val rand = new scala.util.Random(15269182L)
    val entries = rand.shuffle(
      readQASRLFile(scala.io.Source.fromFile("resources/qasrl/propbank-qasrl.txt").getLines.toList)
    ).take(100) ++ rand.shuffle(
      readQASRLFile(scala.io.Source.fromFile("resources/qasrl/wiki1.train.qa").getLines.toList)
    ).take(100)
    entries.toMap
  }

  lazy val Wiktionary = new WiktionaryFileSystemService(
    resourcePath.resolve("wiktionary")
  )

  implicit object SentenceIdHasAlignedTokens extends HasTokens[SentenceId] {
    override def getTokens(id: SentenceId): Vector[String] = qasrlSentences(id)
  }

  lazy val allIds = {
    val rand = new scala.util.Random(38252629L)
    rand.shuffle(qasrlSentences.keys.toVector)
  }

  implicit lazy val inflections = {
    val tokens = for {
      id <- allIds.iterator
      word <- id.tokens.iterator
    } yield word
    Wiktionary.getInflectionsForTokens(tokens)
  }

  def numGenerationAssignmentsForPrompt(p: QASRLGenerationPrompt[SentenceId]) = 1

  lazy val experiment = new QASRLAnnotationPipeline(
    allIds, numGenerationAssignmentsForPrompt,
    liveAnnotationDataService,
    frozenGenerationHITTypeId = frozenGenerationHITTypeId,
    frozenValidationHITTypeId = frozenValidationHITTypeId,
    generationAccuracyDisqualTypeLabel = None,
    generationCoverageDisqualTypeLabel = None,
    validationAgreementDisqualTypeLabel = None)

  def dataExporter = new AnnotationDataExporter(experiment)
  def dataset = dataExporter.dataset(SentenceId.toString(_), identity[String](_))

  def genInfos = experiment.allGenInfos
  def valInfos = experiment.allValInfos

  def workerAnonymizationMap: Map[String, String] = {
    val allGenWorkerIdsIter = for {
      HITInfo(_, assignments) <- genInfos.iterator
      a <- assignments
    } yield a.workerId

    val allValWorkerIdsIter = for {
      HITInfo(_, assignments) <- valInfos.iterator
      a <- assignments
    } yield a.workerId

    val allWorkerIds = (allGenWorkerIdsIter ++ allValWorkerIdsIter).toSet

    val rand = new Random(1543754734L)
    val randomOrderedWorkerIds = rand.shuffle(allWorkerIds.toVector)
    randomOrderedWorkerIds.zipWithIndex.map {
      case (workerId, index) => workerId -> index.toString
    }.toMap
  }

  import qasrl.data._

  def writeDataset(dataset: Dataset) = {
    import io.circe.Printer
    import io.circe.syntax._
    import qasrl.data.JsonCodecs._
    saveOutputFile(
      s"$label.jsonl",
      dataset.sentences.toVector.sortBy(_._1)
        .map(_._2.asJson)
        .map(Printer.noSpaces.pretty)
        .mkString("\n")
    )
  }
}
