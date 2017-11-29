package example.papose

import cats.data.NonEmptyList
import cats.implicits._

import turksem.FileSystemAnnotationDataService
import turksem.gapfill.GapfillPrompt
import turksem.gapfill.GapfillAnnotationPipeline
import turksem.util._

import nlpdata.util.Text
import nlpdata.util.HasTokens.ops._

import java.nio.file.{Files, Paths}

import scala.util.Try

import spacro.tasks.TaskConfig

import upickle.default._

import turksem.posey._

class AnnotationSetup(label: String)(implicit config: TaskConfig) {

  import example.emnlp2017.Datasets
  import example.emnlp2017.SentenceId
  import example.emnlp2017.SentenceIdHasTokens
  import example.emnlp2017.inflections
  import example.emnlp2017.allIds

  val staticDataPath = Paths.get(s"data/papose/$label/static")

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
    Paths.get(s"data/papose/$label/live/production")
  } else {
    Paths.get(s"data/papose/$label/live/sandbox")
  }
  implicit val liveAnnotationDataService = new FileSystemAnnotationDataService(liveDataPath)

  def readPoseyPrompt(line: String): Option[PoseyPrompt[SentenceId]] = {
    (line.trim.split("\t").toList: @unchecked) match {
      case fileId :: sentenceNum :: pairStrings =>
        val sid = SentenceId.fromString(s"$fileId:$sentenceNum".replaceAll("/", ":").replaceAll(".txt", ""))
        NonEmptyList.fromList(pairStrings).map(nePairStrings =>
          PoseyPrompt[SentenceId](
            sid,
            nePairStrings.map(s =>
              s.split(" ").map(_.split("-").toList.map(_.toInt)).toList match {
                case List(begin1, end1) :: List(begin2, end2) :: Nil =>
                  ContiguousSpan(begin1, end1 - 1) -> ContiguousSpan(begin2, end2 - 1)
              }
            )
          )
        )
    }
  }

  lazy val trainMissing = loadInputFile("train-missing.tsv").get.flatMap(readPoseyPrompt).toVector
  lazy val devMissing   = loadInputFile(  "dev-missing.tsv").get.flatMap(readPoseyPrompt).toVector
  lazy val testMissing  = loadInputFile( "test-missing.tsv").get.flatMap(readPoseyPrompt).toVector

  lazy val allPrompts = weightedRoundRobin(List(trainMissing, devMissing, testMissing))

  // for viewing debuggy

  lazy val qasById = Datasets.train.sentenceToQAs.toVector
  lazy val missingById = allPrompts.groupBy(_.sid).map { case (id, entries) => id -> entries.head } // id should be unique anyway
  def checkDataValidity = {
    println(if(allPrompts.groupBy(_.sid).forall(_._2.size == 1)) "IDs are unique." else "IDs are not unique! :(")
    println(allPrompts.find(p => Try(p.sid.tokens).isFailure).fold("All IDs are valid.")(id => s"Invalid ID! :( first one: $id"))
  }
  def viewTrainExample(n: Int): Boolean = {
    val (sid, sqas) = qasById(n)
    val sentenceTokens = sid.tokens
    println(Text.render(sentenceTokens))
    for(sqa <- sqas) {
      val answerString = sqa.answers.map(is => Text.renderSpan(sentenceTokens, is)).mkString("\t")
      println(f"\t${sqa.question}%-50s --> ${answerString}%s")
    }
    println("Missing pairs:")
    missingById.get(sid).fold(println("\tNo missing pairs for this sentence."))(record =>
      record.pairs.toList.foreach { case (s1, s2) =>
        println(Text.renderSpan(sentenceTokens, s1.indices) + " <--> " + Text.renderSpan(sentenceTokens, s2.indices))
      }
    )
    missingById.get(sid).nonEmpty
  }

  // lazy val experiment = new GapfillAnnotationPipeline[SentenceId, SentenceQGuesser, QGuesser](
  //   allPrompts, QGuesser(
  //     templateAnalysis.starterTemplateProbsByCoarseGrainedLabel,
  //     templateAnalysis.questionExpansionProbabilities))
}
