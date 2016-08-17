package mts.experiments.expB

import mts.experiments._
import mts.experiments.expA._
import mts.core._
import mts.util._
import mts.tasks._
import mts.conll._
import mts.qa._

import akka.actor._

import scala.util.Try
import scala.util.Random

object ValidationExperiment {
  // file names should have b_ prepended so experiments have an obvious order
  val experimentName = "b_validation"

  type QuestionData = ValidationPrompt // path to sentence, original question
  type AnswerData = ValidationResponse // answers (+ invalid question flag), feedback

  lazy val questionData: List[QuestionData] = {
    val rand = new Random(42) // DO NOT CHANGE THE SEED
    // it has to be consistent so that after restarts the set of questions will be the same.
    val validationQuestions = for {
      annotation <- OpenFormExperiment.getAllAnnotations()
      (path, _) = OpenFormExperiment.protoQASpec.extractQuestionData(annotation.question.get)
      (qaPairs, _) = OpenFormExperiment.protoQASpec.extractAnswerData(annotation.answer)
      (q, a) <- qaPairs
    } yield ValidationQuestion(path, annotation.workerId, q, a)
    val validationPrompts = validationQuestions.groupBy(_.path).toList.flatMap {
      case (path, vQuestions) => rand.shuffle(vQuestions).grouped(3).map(g => ValidationPrompt(path, g.toList))
    }
    rand.shuffle(validationPrompts).toList
  }

  lazy val taskSpec = ValidationTask(numAssignmentsPerHIT = 3)

  lazy val system = ActorSystem("system")

  lazy val actor = taskSpec.createMonitor(system, questionData.iterator, 150)

  def start() = actor ! taskSpec.Message.Start
  def stop() = actor ! taskSpec.Message.Stop
  def disable() = actor ! taskSpec.Message.Disable
  def expire() = actor ! taskSpec.Message.Expire
  def update() = actor ! taskSpec.Message.Update

  // TODO save HIT types and access them here and such
  def getAllAnnotations(): List[Annotation] =
    FileManager.loadAnnotationsForHITType(taskSpec.hitType)

  def getAllQAPairs(): Iterable[(QuestionData, List[AnswerData])] =
    taskSpec.annotatedQAPairs.toMap.values

  def getAllFeedback(): Iterable[String] = getAllQAPairs()
    .flatMap(_._2.map(_.feedback))
    .filterNot(_.isEmpty)

  case class Validation(
    workerId: String,
    validationAnswer: ValidationAnswer)

  case class QAValidation(
    vQuestion: ValidationQuestion,
    vAnswers: List[Validation])

  def qaValidationsFromAnnotations(annotations: List[Annotation]) = {
    val annotations = getAllAnnotations().filter(!_.question.isEmpty)
    val instances = for {
      anno <- annotations
      (ValidationPrompt(path, qs), ValidationResponse(as, _)) <- taskSpec.qaSpec.getQAPair(anno).toList
      (q, a) <- qs.zip(as)
    } yield (q, Validation(anno.workerId, a))
    val qaValidations = instances.groupBy(_._1).map {
      case (vQuestion, pairList) => QAValidation(vQuestion, pairList.map(_._2))
    }
    qaValidations
  }

  def readableQATSV(): String = {
    val annotations = getAllAnnotations().filter(!_.question.isEmpty)
    val qaValidations = qaValidationsFromAnnotations(annotations)
    val qaValidationsBySentence = qaValidations.groupBy(_.vQuestion.path)
    val sb = new java.lang.StringBuilder()
    for {
      (path, qaValidations) <- qaValidationsBySentence
      sentence <- FileManager.getCoNLLSentence(path).toOptionPrinting.toList
      _ = sb.append(s"\n${TextRendering.renderSentence(sentence)}\n")
      QAValidation(vQuestion, validations) <- qaValidations
      validationString = validations.map {
        case Validation(workerId, vAnswer) => s"\t$vAnswer\t$workerId\n"
      }.mkString
      _ = sb.append(s"${vQuestion.question}\t${vQuestion.answer}\t${vQuestion.workerId}\n$validationString")
    } yield ()
    sb.toString
  }
}
