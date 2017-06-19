package qamr.aristo

import qamr._
import qamr.annotation._
import qamr.util._

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

import scala.util.Random
import scala.util.Try

import upickle.default._

class AristoAnnotationSetup(implicit config: TaskConfig) {

  import config._

  import java.nio.file.{Paths, Path, Files}
  private[this] val dataPath = Paths.get("live-data/aristo")
  val liveAnnotationDataService = new FileSystemAnnotationDataService(dataPath)

  lazy val sourceIds: Vector[Ai2SentenceId] = {
    val desiredMathProblemSentenceIds = {
      val hosseiniIds = mathProblemSentenceIds.filter { mpId =>
        allMathProblems(mpId.problemId).tags.contains("hosseini-ma2")
      }
      val sigmadolphinIds = mathProblemSentenceIds.filter { mpId =>
        allMathProblems(mpId.problemId).tags.contains("sigmadolphin-dev")
      }
      val commonCoreIds = mathProblemSentenceIds.filter { mpId =>
        val problem = allMathProblems(mpId.problemId)
        problem.tags.contains("commoncore") && problem.id >= 0 && problem.id < 100
      }
      hosseiniIds.take(33) ++ sigmadolphinIds.take(33) ++ commonCoreIds.take(34)
    }
    kbNonDuplicateSentenceIds ++ desiredMathProblemSentenceIds
  }

  lazy val numSentences: Int = sourceIds.size

  lazy val (train, dev, test) = {
    val shuffleRand = new Random(832592735L)
    val (train, devTestRest) = shuffleRand.shuffle(sourceIds).splitAt(numSentences * 4 / 5)
    val (dev, testRest) = devTestRest.splitAt(numSentences / 10)
    val test = testRest.take(numSentences / 10)
    (train, dev, test)
  }

  val allIds = train ++ dev ++ test

  implicit lazy val inflections = {
    val tokens = for {
      id <- allIds.iterator
      word <- id.tokens.iterator
    } yield word
    Wiktionary.getInflectionsForTokens(tokens)
  }

  // seems reasonable
  def numGenerationAssignmentsForPrompt(p: GenerationPrompt[Ai2SentenceId]) = 3

  lazy val experiment = new AnnotationPipeline(
    allIds, numGenerationAssignmentsForPrompt,
    liveAnnotationDataService,
    IsStopword(isReallyUninteresting),
    AristoQualTest,
    validationTestQualTypeLabel = Some("round 2"))
}
