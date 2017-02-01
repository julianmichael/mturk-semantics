package mts.experiments.expF

import mts.experiments._
import mts.core._
import mts.tasks._
import mts.tasks._
import mts.conll._
import mts.language._
import mts.util._
import mts.util.LowerCaseStrings._

import akka.actor._
import akka.stream.scaladsl.Flow

import scala.concurrent.duration._
import scala.language.postfixOps

import monocle._
import monocle.macros._

import upickle.default._

class ValidationExperiment(implicit config: TaskConfig) {
  val experimentName = "f_validation"

  val answerValidationHITType = HITType(
    title = s"Answer simple questions about a sentence",
    description = s"""
      Given a sentence and a list of questions,
      highlight the part of the sentence that answers the question.
    """.trim,
    reward = 0.10,
    keywords = "language,english,question answering")

  lazy val sentenceApiFlow = Flow[ApiRequest].map {
    case SentenceRequest(path) => SentenceResponse(FileManager.getCoNLLSentence(path).get)
  }

  val sampleValidationPrompt = ValidationPrompt(
    sentences.head._1, List(
      SourcedQAPair("", "Who is awesome?", Set(1, 2, 3, 4)),
      SourcedQAPair("", "What did Julian do?", Set(5, 6, 8, 9))))

  lazy val answerValidationTaskSpec = TaskSpecification[ValidationPrompt, AnswerValidationResponse, ApiRequest, ApiResponse](
    TaskIndex.expFAnswerValidationTaskKey, answerValidationHITType, sentenceApiFlow, sampleValidationPrompt)

  val questionValidationHITType = HITType(
    title = s"Simplify questions about a sentence",
    description = s"""
      You'll be given a sentence and a list of questions (and their answers).
      Revise and simplify the questions while preserving their answers.
    """.trim,
    reward = 0.10,
    keywords = "language,english,question answering")

  lazy val questionValidationTaskSpec = TaskSpecification[ValidationPrompt, QuestionValidationResponse, ApiRequest, ApiResponse](
    TaskIndex.expFQuestionValidationTaskKey, questionValidationHITType, sentenceApiFlow, sampleValidationPrompt)

  import config.actorSystem
  lazy val server = new Server(
    List(answerValidationTaskSpec, questionValidationTaskSpec))

  // get all of the questions to validate from expE

  import expE.{QuestionWordExperiment, QAGenPrompt, QAGenResponse}
  lazy val experimentE = new QuestionWordExperiment
  lazy val expEHITInfos = experimentE.loadQAGenData.map(GenIso.fields[HITInfo[QAGenPrompt, QAGenResponse]].reverseGet)

  lazy val questionInfos = for {
    HITInfo(hit, assignments) <- expEHITInfos
    assignment <- assignments
    (question, answer) <- assignment.response.qaPairs
    if !question.isEmpty
  } yield (hit.prompt.path, SourcedQAPair(hit.hitId, question, answer))

  // group the questions by which sentence they're asking about,
  // randomizing (in a repeatable way) the order of each sentence's QA pairs

  lazy val sourcedQAPairsBySentence = {
    val shuffleRand = new util.Random(555555555L)
    questionInfos.groupBy(_._1).map {
      case (path, items) => path -> shuffleRand.shuffle(items.map(_._2))
    }
  }

  // for each sentence, group its questions into lists of 10 each; then, randomize these sentence/group pairs
  val numQAsPerHIT = 10

  lazy val prompts = {
    val shuffleRand = new util.Random(444443333L)

    val inOrder = for {
      (path, qaPairs) <- sourcedQAPairsBySentence
      qaPairGroup <- qaPairs.grouped(numQAsPerHIT)
    } yield ValidationPrompt(path, qaPairGroup.toList)

    shuffleRand.shuffle(inOrder)
  }

  lazy val qvHITManager = new NumAssignmentsHITManager(
    questionValidationTaskSpec,
    numAssignmentsPerPrompt = (if(config.isProduction) 2 else 1),
    numHITsToKeepActive = (if(config.isProduction) 30 else 3),
    prompts.iterator)
  lazy val qvActor = actorSystem.actorOf(Props(TaskManager(qvHITManager)))

  lazy val avHITManager = new NumAssignmentsHITManager(
    answerValidationTaskSpec,
    numAssignmentsPerPrompt = (if(config.isProduction) 2 else 1),
    numHITsToKeepActive = (if(config.isProduction) 30 else 3),
    prompts.iterator)
  lazy val avActor = actorSystem.actorOf(Props(TaskManager(avHITManager)))

  def start(interval: FiniteDuration = 1 minute) = {
    server
    qvActor ! qvHITManager.Message.Start(interval)
    avActor ! avHITManager.Message.Start(interval)
  }
  def stop() = {
    qvActor ! qvHITManager.Message.Stop
    avActor ! avHITManager.Message.Stop
  }
  def disable() = {
    qvActor ! qvHITManager.Message.Disable
    avActor ! avHITManager.Message.Disable
  }
  def expire() = {
    qvActor ! qvHITManager.Message.Expire
    avActor ! avHITManager.Message.Expire
  }
  def update() = {
    server
    qvActor ! qvHITManager.Message.Update
    avActor ! avHITManager.Message.Update
  }
}
