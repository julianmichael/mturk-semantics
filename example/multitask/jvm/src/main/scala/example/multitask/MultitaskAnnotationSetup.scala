package example.multitask

import cats._
import cats.implicits._

import turksem.FileSystemAnnotationDataService
import turksem.qamr._
import turksem.qasrl._
import turksem.util._

import turkey._
import turkey.tasks._

import nlpdata.structure._
import nlpdata.datasets.conll
import nlpdata.datasets.wiktionary.Inflections
import nlpdata.util.LowerCaseStrings._
import nlpdata.util.Text
import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.datasets.ptb3._
import nlpdata.datasets.qasrl
import nlpdata.datasets.wiktionary
import nlpdata.datasets.wiktionary.Inflections
import turksem.IsStopword

import akka.actor._
import akka.stream.scaladsl.Flow
import akka.stream.scaladsl.Source

import scala.concurrent.duration._
import scala.language.postfixOps

import scala.util.Try

import upickle.default._

import edu.stanford.nlp.ling.Word
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory
import java.io.StringReader
import java.nio.file.{Files, Path, Paths}

import scala.util.Try
import scala.util.Random

import upickle.default._


class MultitaskAnnotationSetup(
  val label: String = "final",
  frozenGenerationHITTypeId: Option[String] = None,
  frozenValidationHITTypeId: Option[String] = None)(
  implicit config: TaskConfig) {

  val resourcePath = java.nio.file.Paths.get("resources")

  // ignore file system errors.. the service should always succeed
  lazy val PTB = {
    new InterpretedPTB3Service(
      λ[Try ~> Id](_.get) compose (new PTB3FileSystemInterpreter(resourcePath.resolve("ptb3")))
    )
  }

  lazy val Wiktionary = new wiktionary.WiktionaryFileSystemService(
    resourcePath.resolve("wiktionary")
  )

  lazy val CoNLL = new conll.CoNLLFileSystemService(
    resourcePath.resolve("conll-2012")
  ).interpretThrough(λ[Try ~> Id](_.get))

  implicit object SentenceIdHasTokens extends HasTokens[SentenceId] {
    def getTokens(id: SentenceId): Vector[String] = id match {
      case PTBSentenceId(path) => PTB.getSentence(path).tokens
      case CoNLLSentenceId(path) => CoNLL.getSentence(path).tokens
    }
  }

  import java.nio.file.{Paths, Path, Files}
  private[this] val liveDataPath = if(config.isProduction) {
    Paths.get(s"live-data/multitask/$label")
  } else {
    Paths.get(s"live-data/multitask/$label")
  }
  val liveAnnotationDataService = new FileSystemAnnotationDataService(liveDataPath)

  val staticDataPath = Paths.get(s"static-data/multitask/$label")

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

  lazy val (brownTrain, brownDev, brownTest) = {
    val unshuffled = for {
      path @ BrownPath("CK", _) <- PTB.getAllPaths
      sentence <- PTB.getFile(path).sentences
    } yield PTBSentenceId(sentence.path)
    val shuffleRand = new Random(234348765L)
    val shuffled = shuffleRand.shuffle(unshuffled.toVector)
    val trainBrown = shuffled.filter(SentenceId.isBrownTrain)
    val devBrown = shuffled.filter(SentenceId.isBrownDev)
    val testBrown = shuffled.filter(SentenceId.isBrownTest)
    (trainBrown, devBrown, testBrown)
  }
  val brownIds = weightedRoundRobin(List(brownTrain, brownDev, brownTest))

  lazy val (ontoNotesTrain, ontoNotesDev, ontoNotesTest) = {
    lazy val unshuffled = for {
      path <- CoNLL.getAllPaths
      if path.language == "english" && path.domain == "bc"
      sentence <- CoNLL.getFile(path).sentences
    } yield CoNLLSentenceId(sentence.path)
    val shuffleRand = new Random(425632738L)
    val shuffled = shuffleRand.shuffle(unshuffled.toVector)
    val trainOntoNotes = shuffled.filter(_.path.filePath.split == "train")
    val devOntoNotes = shuffled.filter(_.path.filePath.split == "development")
    val testOntoNotes = shuffled.filter(_.path.filePath.split == "test")
    (trainOntoNotes, devOntoNotes, testOntoNotes)
  }
  val ontoNotesIds = weightedRoundRobin(List(ontoNotesTrain, ontoNotesDev, ontoNotesTest))

  lazy val allIds: Vector[SentenceId] = brownIds ++ ontoNotesIds

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
    generationAccuracyDisqualTypeLabel = Some("v3-templates"),
    generationCoverageDisqualTypeLabel = Some("v3-templates"),
    validationAgreementDisqualTypeLabel = Some("v3-templates"))

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

  def writeAllTSVs {
    saveAnnotationData("brown-train", brownTrain)
    saveAnnotationData("brown-dev", brownDev)
    saveAnnotationData("brown-test", brownTest)
    saveAnnotationData("bc-train", ontoNotesTrain)
    saveAnnotationData("bc-dev", ontoNotesDev)
    saveAnnotationData("bc-test", ontoNotesTest)
  }
}
