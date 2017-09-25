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


class MultitaskAnnotationSetup(implicit config: TaskConfig) {

  val label = "final"

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
    Paths.get(s"live-data/multitask/$label/production")
  } else {
    Paths.get(s"live-data/multitask/$label/sandbox")
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

  lazy val allBrownPaths = for {
    path @ BrownPath("CK", _) <- PTB.getAllPaths
    sentence <- PTB.getFile(path).sentences
  } yield PTBSentenceId(sentence.path)

  lazy val allOntoNotesPaths = for {
    path <- CoNLL.getAllPaths
    if path.language == "english" && path.domain == "bc"
    sentence <- CoNLL.getFile(path).sentences
  } yield CoNLLSentenceId(sentence.path)

  lazy val allIds: Vector[SentenceId] = {
    import scala.annotation.tailrec
    @tailrec def weightedRoundRobinAux[A](soFar: Vector[A], vectors: List[Vector[A]]): Vector[A] = {
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

    val brownShuffleRand = new Random(234348765L)
    val brownPaths = brownShuffleRand.shuffle(allBrownPaths.toVector)
    val trainBrown = brownPaths.filter(SentenceId.isBrownTrain)
    val devBrown = brownPaths.filter(SentenceId.isBrownDev)
    val testBrown = brownPaths.filter(SentenceId.isBrownTest)
    val finalBrown = weightedRoundRobin(List(trainBrown, devBrown, testBrown))

    val ontoNotesShuffleRand = new Random(425632738L)
    val ontoNotesPaths = ontoNotesShuffleRand.shuffle(allOntoNotesPaths.toVector)
    val trainOntoNotes = ontoNotesPaths.filter(_.path.filePath.split == "train")
    val devOntoNotes = ontoNotesPaths.filter(_.path.filePath.split == "development")
    val testOntoNotes = ontoNotesPaths.filter(_.path.filePath.split == "test")
    val finalOntoNotes = weightedRoundRobin(List(trainOntoNotes, devOntoNotes, testOntoNotes))

    finalBrown ++ finalOntoNotes
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
    generationAccuracyDisqualTypeLabel = Some("v3-templates"),
    generationCoverageDisqualTypeLabel = Some("v3-templates"),
    validationAgreementDisqualTypeLabel = Some("v3-templates"))
}
