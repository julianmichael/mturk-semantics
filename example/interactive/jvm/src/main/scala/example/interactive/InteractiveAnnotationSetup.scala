package example.interactive

import turksem.util._
import turksem.iqa._

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

  def numGenerationAssignmentsForPrompt(p: IQAPrompt[SentenceId]) = 1

  lazy val allPrompts = allIds.map(IQAPrompt(_))

  lazy val experiment = new IQAAnnotationPipeline(allPrompts)
}
