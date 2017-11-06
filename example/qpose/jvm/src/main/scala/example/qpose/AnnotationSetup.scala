package example.qpose

import turksem.FileSystemAnnotationDataService
import turksem.gapfill.GapfillPrompt
import turksem.gapfill.GapfillAnnotationPipeline

import java.nio.file.{Files, Paths}

import scala.util.Try

import turkey.tasks.TaskConfig

import upickle.default._

class AnnotationSetup(label: String)(implicit config: TaskConfig) {

  import example.emnlp2017.Datasets
  import example.emnlp2017.SentenceId
  import example.emnlp2017.SentenceIdHasTokens
  import example.emnlp2017.inflections
  import example.emnlp2017.allIds

  val staticDataPath = Paths.get(s"data/qpose/$label/static")

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

  private[this] val liveDataPath = if(config.isProduction) {
    Paths.get(s"data/qpose/$label/live/production")
  } else {
    Paths.get(s"data/qpose/$label/live/sandbox")
  }
  implicit val liveAnnotationDataService = new FileSystemAnnotationDataService(liveDataPath)

  val templateAnalysis = new TemplateAnalysis(label, Datasets.trainDevPTB)

  lazy val allPrompts = allIds.map(GapfillPrompt(_))

  // very annoying that I have to do this...
  import turksem.gapfill.LowerCaseStringSerialization._
  implicit val sqgReader = SentenceQGuesser.sentenceQGuesserReader
  implicit val sqgWriter = SentenceQGuesser.sentenceQGuesserWriter

  lazy val experiment = new GapfillAnnotationPipeline[SentenceId, SentenceQGuesser, QGuesser](
    allPrompts, QGuesser(
      templateAnalysis.starterTemplateProbsByCoarseGrainedLabel,
      templateAnalysis.questionExpansionProbabilities))
}
