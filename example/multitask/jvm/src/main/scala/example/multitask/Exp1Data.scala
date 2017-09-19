package example.multitask

import cats.~>
import cats.Id
import cats.implicits._

import java.nio.file.Paths
import java.nio.file.Files

import scala.util.Try

import turksem.qamr.GenerationPrompt
import turksem.qasrl._
import turksem.util._

import turkey._
import turkey.tasks._

import nlpdata.util.HasTokens
import nlpdata.util.HasTokens.ops._
import nlpdata.datasets.ptb3._

import akka.pattern.ask
import scala.concurrent.duration._

class Exp1Data {

  val resourcePath = java.nio.file.Paths.get("resources")

  val PTB = {
    val getTry = new (Try ~> Id) {
      def apply[A](a: Try[A]): Id[A] = a.get
    }
    new InterpretedPTB3Service(
      getTry compose (new PTB3FileSystemInterpreter(resourcePath.resolve("ptb3")))
    )
  }

  implicit object PTB3SentencePathHasTokens extends HasTokens[PTB3SentencePath] {
    def getTokens(id: PTB3SentencePath): Vector[String] = PTB.getSentence(id).tokens
  }

  val annotationPath = java.nio.file.Paths.get("annotations")
  val hitDataService = new FileSystemHITDataService(annotationPath.resolve("sandbox-multitask-exp1"))

  val smallGenHITTypeId = "33RDX9IX09HK2UG36D9ZX0GNORFQ3N"
  val largeGenHITTypeId = "3SJ5GB440G65KQZ76REI758T2RBQ48"
  val valHITTypeId = "3KBOIXB475RNG937KWXSCXE72N6Q5D"

  val staticDataPath = Paths.get("static-data/multitask/exp1")

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

  def allSmallGenInfos = hitDataService.getAllHITInfo[GenerationPrompt[PTB3SentencePath], List[VerbQA]](smallGenHITTypeId).get
  def allLargeGenInfos = hitDataService.getAllHITInfo[GenerationPrompt[PTB3SentencePath], List[VerbQA]](largeGenHITTypeId).get
  def allGenInfos = allSmallGenInfos ++ allLargeGenInfos

  def allValInfos = hitDataService.getAllHITInfo[QASRLValidationPrompt[PTB3SentencePath], List[QASRLValidationAnswer]](valHITTypeId).get

  def writeReadableTSV = {
    lazy val allIds = {
      val allPaths = PTB.getAllPaths
      val eligibleBrownPaths = allPaths.collect {
        case p @ BrownPath("CK", number) if number > 3 => p
      }
      val eligibleSentencePaths = for {
        path <- eligibleBrownPaths
        sentence <-  PTB.getFile(path).sentences
      } yield sentence.path
      eligibleSentencePaths.toVector.take(100).sorted.toList
    }

    def writeId(id: PTB3SentencePath) = id.toString
    val tsv = DataIO.makeReadableQAPairTSV[PTB3SentencePath](
      ids = allIds,
      writeId = writeId,
      anonymizeWorker = identity, // TODO
      genInfos = allGenInfos,
      valInfos = allValInfos)
    saveOutputFile("readable.tsv", tsv)
  }
}

object Exp1Data {
  def init = new Exp1Data
}
