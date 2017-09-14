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
import nlpdata.datasets.wiki1k._
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

  implicit object SentenceIdHasTokens extends HasTokens[SentenceId] {
    def getTokens(id: SentenceId): Vector[String] = PTB3.getSentence(id).tokens
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
  val PTB3 = {
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

  val QASRL = new qasrl.QASRLFileSystemService(
    resourcePath.resolve("qasrl"),
    new nlpdata.datasets.ptb.PTBFileSystemService(resourcePath.resolve("ptb"))
  )

  val ptb3QASRLPaths = QASRL.allQASRLPaths.flatMap(PTB3SentencePath.fromPTBSentencePath)

  // for Brown sentences
  // lazy val allIds = {
  //   val allPaths = PTB3.getAllPaths
  //   val eligibleBrownPaths = allPaths.collect {
  //     case p @ BrownPath("CK", number) if number > 3 => p
  //   }
  //   val eligibleSentencePaths = for {
  //     path <- eligibleBrownPaths
  //     sentence <-  PTB3.getFile(path).sentences
  //   } yield sentence.path
  //   eligibleSentencePaths.toVector.take(100)
  // }

  // just 100 PTB sentences that overlap with QA-SRL
  lazy val allIds = ptb3QASRLPaths.drop(100).take(100)

  implicit lazy val inflections = {
    val tokens = for {
      id <- allIds.iterator
      word <- id.tokens.iterator
    } yield word
    Wiktionary.getInflectionsForTokens(tokens)
  }

  def numGenerationAssignmentsForPrompt(p: GenerationPrompt[SentenceId]) = 1

  lazy val experiment = new QASRLAnnotationPipeline(
    allIds, numGenerationAssignmentsForPrompt,
    liveAnnotationDataService,
    generationAccuracyQualTypeLabel = Some("v3-templates"),
    generationCoverageQualTypeLabel = Some("v3-templates"),
    validationAgreementQualTypeLabel = Some("v3-templates"))
}
