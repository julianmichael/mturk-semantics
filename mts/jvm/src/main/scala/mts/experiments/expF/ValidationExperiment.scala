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
    reward = 0.20,
    keywords = "language,english,question answering")

  lazy val sentenceApiFlow = Flow[ApiRequest].map {
    case SentenceRequest(path) => SentenceResponse(FileManager.getCoNLLSentence(path).get)
  }

  val sampleValidationPrompt = ValidationPrompt(
    sentences.head._1, List(
      SourcedQAPair("", "Who is awesome?", Set(1, 2, 3, 4)),
      SourcedQAPair("", "What did Julian do?", Set(5, 6, 8, 9))))

  lazy val avTaskSpec = TaskSpecification[ValidationPrompt, AnswerValidationResponse, ApiRequest, ApiResponse](
    TaskIndex.expFAnswerValidationTaskKey, answerValidationHITType, sentenceApiFlow, sampleValidationPrompt,
    frozenHITTypeId = Some("35NE15Q62B2FVT9IPT6KFM3QDZDT7K"))

  val questionValidationHITType = HITType(
    title = s"Simplify questions about a sentence",
    description = s"""
      You'll be given a sentence and a list of questions (and their answers).
      Revise and simplify the questions while preserving their answers.
    """.trim,
    reward = 0.30,
    keywords = "language,english,question answering")

  lazy val qvTaskSpec = TaskSpecification[ValidationPrompt, QuestionValidationResponse, ApiRequest, ApiResponse](
    TaskIndex.expFQuestionValidationTaskKey, questionValidationHITType, sentenceApiFlow, sampleValidationPrompt,
    frozenHITTypeId = Some("3SGI3JPMZ02SE0U77QDB3KAL5GWFBK"))

  val longAnswerValidationHITType = HITType(
    title = s"Answer simple questions about a sentence",
    description = s"""
      Given a sentence and a list of questions,
      highlight the longest part the sentence that correctly answers the question.
    """.trim,
    reward = 0.20,
    keywords = "language,english,question answering")
  lazy val lavTaskSpec = TaskSpecification[ValidationPrompt, AnswerValidationResponse, ApiRequest, ApiResponse](
    TaskIndex.expFLongAnswerValidationTaskKey, longAnswerValidationHITType, sentenceApiFlow, sampleValidationPrompt)

  import config.actorSystem
  lazy val server = new Server(
    List(avTaskSpec, qvTaskSpec, lavTaskSpec))

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
  val numQAsPerHIT = 6

  lazy val prompts = {
    val shuffleRand = new util.Random(444443333L)

    val inOrder = for {
      (path, qaPairs) <- sourcedQAPairsBySentence
      qaPairGroup <- qaPairs.grouped(numQAsPerHIT)
    } yield ValidationPrompt(path, qaPairGroup.toList)

    shuffleRand.shuffle(inOrder)
  }

  lazy val qvHITManager = new NumAssignmentsHITManager(
    qvTaskSpec,
    numAssignmentsPerPrompt = (if(config.isProduction) 2 else 1),
    numHITsToKeepActive = (if(config.isProduction) 30 else 3),
    prompts.iterator)
  lazy val qvActor = actorSystem.actorOf(Props(TaskManager(qvHITManager)))

  lazy val avHITManager = new NumAssignmentsHITManager(
    avTaskSpec,
    numAssignmentsPerPrompt = (if(config.isProduction) 2 else 1),
    numHITsToKeepActive = (if(config.isProduction) 30 else 3),
    prompts.iterator)
  lazy val avActor = actorSystem.actorOf(Props(TaskManager(avHITManager)))

  // oh, ok, guess we should do longest-answer preferring validation too

  lazy val lavHITManager = new NumAssignmentsHITManager(
    lavTaskSpec,
    numAssignmentsPerPrompt = (if(config.isProduction) 2 else 1),
    numHITsToKeepActive = (if(config.isProduction) 30 else 3),
    prompts.iterator)
  lazy val lavActor = actorSystem.actorOf(Props(TaskManager(lavHITManager)))

  def start(interval: FiniteDuration = 1 minute) = {
    server
    qvActor ! qvHITManager.Message.Start(interval)
    avActor ! avHITManager.Message.Start(interval)
    lavActor ! lavHITManager.Message.Start(interval)
  }
  def stop() = {
    qvActor ! qvHITManager.Message.Stop
    avActor ! avHITManager.Message.Stop
    lavActor ! lavHITManager.Message.Stop
  }
  def disable() = {
    qvActor ! qvHITManager.Message.Disable
    avActor ! avHITManager.Message.Disable
    lavActor ! lavHITManager.Message.Disable
  }
  def expire() = {
    qvActor ! qvHITManager.Message.Expire
    avActor ! avHITManager.Message.Expire
    lavActor ! lavHITManager.Message.Expire
  }
  def update() = {
    server
    qvActor ! qvHITManager.Message.Update
    avActor ! avHITManager.Message.Update
    lavActor ! lavHITManager.Message.Update
  }

  lazy val qData = FileManager.loadAllData[ValidationPrompt, QuestionValidationResponse](qvTaskSpec.hitTypeId)
  lazy val aData = FileManager.loadAllData[ValidationPrompt, AnswerValidationResponse](avTaskSpec.hitTypeId)
  lazy val laData = FileManager.loadAllData[ValidationPrompt, AnswerValidationResponse](lavTaskSpec.hitTypeId)

  case class ValidatedQAPair(
    origQuestion: String,
    origAnswer: Set[Int],
    newQuestions: List[Option[String]],
    newShortAnswers: List[Option[Set[Int]]],
    newLongAnswers: List[Option[Set[Int]]])

  lazy val pathToHITToQAPairs: Map[CoNLLSentencePath, Map[String, List[ValidatedQAPair]]] =
    prompts.groupBy(_.path).map {
      case (path, pathPrompts) =>
        val hitToQAPairs = pathPrompts
          .flatMap(_.sourcedQAPairs)
          .groupBy(_.originalHITId).map {
          case (hitId, hitQAPairs) =>
            val validatedQAPairs = hitQAPairs.map {
              case sqa @ SourcedQAPair(_, question, answer) =>
                val validationQuestions = qData.iterator
                  .filter(_._1.prompt.sourcedQAPairs.contains(sqa))
                  .map {
                  case (hit, assignments) =>
                    val index = hit.prompt.sourcedQAPairs.indexOf(sqa)
                    assignments.map(_.response.questions(index))
                }.flatten
                val validationAnswers = aData.iterator
                  .filter(_._1.prompt.sourcedQAPairs.contains(sqa))
                  .map {
                  case (hit, assignments) =>
                    val index = hit.prompt.sourcedQAPairs.indexOf(sqa)
                    assignments.map(_.response.answerIndices(index))
                }.flatten
                val longValidationAnswers = laData.iterator
                  .filter(_._1.prompt.sourcedQAPairs.contains(sqa))
                  .map {
                  case (hit, assignments) =>
                    val index = hit.prompt.sourcedQAPairs.indexOf(sqa)
                    assignments.map(_.response.answerIndices(index))
                }.flatten
                ValidatedQAPair(question, answer, validationQuestions.toList, validationAnswers.toList,
                                longValidationAnswers.toList)
            }
            hitId -> validatedQAPairs.toList
        }.toMap
        path -> hitToQAPairs
    }.toMap

  def makeTSV: String = {
    val sb = new StringBuilder
    pathToHITToQAPairs.foreach {
      case (path, hitToQAPairs) =>
        val sentence = FileManager.getCoNLLSentence(path).get
        sb.append(TextRendering.renderSentence(sentence) + "\n")
        hitToQAPairs.foreach {
          case (hit, vQAPairs) =>
            vQAPairs.foreach {
              case ValidatedQAPair(question, answerIndices, newQs, newAs, newLAs) =>
                val answer = expE.renderSpan(sentence, answerIndices)
                val renderedQs = newQs.map(_.fold("N/A")(q => q))
                val renderedAs = newAs.map(_.fold("N/A")(a => expE.renderSpan(sentence, a)))
                val renderedLAs = newLAs.map(_.fold("N/A")(a => expE.renderSpan(sentence, a)))
                val answers = (answer :: renderedLAs).zipAll("" :: renderedAs, "", "")
                val allRows = (question :: renderedQs).zipAll(answers, "", "")

                val s = allRows.map {
                  case (q, (la, a)) => s"$q\t$la\t$a\n"
                }.foreach(sb.append)
            }
        }
        sb.append("\n")
    }
    sb.toString
  }

  def writeTSV = FileManager.saveDataFile(experimentName, "readable.tsv", makeTSV)
}
