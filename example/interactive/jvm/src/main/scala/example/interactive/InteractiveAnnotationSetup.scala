package example.interactive

import turksem.FileSystemAnnotationDataService
import turksem.util._
import turksem.iqa._

import spacro._
import spacro.tasks._

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

import scala.util.Try

import java.nio.file.{Files, Path, Paths}

import upickle.default._

class InteractiveAnnotationSetup(label: String)(implicit config: TaskConfig) {

  import example.emnlp2017.SentenceId
  import example.emnlp2017.SentenceIdHasTokens
  import example.emnlp2017.inflections
  import example.emnlp2017.allIds

  val staticDataPath = Paths.get(s"data/interactive/$label/static")

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

  import java.nio.file.{Paths, Path, Files}
  private[this] val liveDataPath = if(config.isProduction) {
    Paths.get(s"data/interactive/$label/live/production")
  } else {
    Paths.get(s"data/interactive/$label/live/sandbox")
  }
  implicit val liveAnnotationDataService = new FileSystemAnnotationDataService(liveDataPath)

  // def numGenerationAssignmentsForPrompt(p: IQAPrompt[SentenceId]) = 1

  lazy val allPrompts = IQAPrompt(
    allIds.head
  ) +: (new scala.util.Random(622437L)).shuffle(allIds.tail.map(IQAPrompt(_)))

  lazy val experiment = new IQAAnnotationPipeline(
    allPrompts,
    CountBasedQuestionGuesser(
      Map.empty[Template, Double]
      // QuestionGuessingSandbox.templatePseudoCounts
      // QuestionGuessingSandbox.templateCorrespondencePseudoCounts
    ))
}
