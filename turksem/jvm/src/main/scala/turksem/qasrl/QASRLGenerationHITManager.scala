package turksem.qasrl

import cats.implicits._

import turkey._
import turkey.tasks._

import turksem._
import turksem.util._
import turksem.qamr.Pring
import turksem.qamr.SaveData

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.services.mturk.model.AssociateQualificationWithWorkerRequest

import upickle.default._

import com.typesafe.scalalogging.StrictLogging

case class FlagBadSentence[SID](id: SID)

class QASRLGenerationHITManager[SID : Reader : Writer](
  helper: HITManager.Helper[QASRLGenerationPrompt[SID], List[VerbQA]],
  validationHelper: HITManager.Helper[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]],
  validationActor: ActorRef,
  coverageDisqualificationTypeId: String,
  // sentenceTrackingActor: ActorRef,
  numAssignmentsForPrompt: QASRLGenerationPrompt[SID] => Int,
  initNumHITsToKeepActive: Int,
  _promptSource: Iterator[QASRLGenerationPrompt[SID]])(
  implicit annotationDataService: AnnotationDataService
) extends NumAssignmentsHITManager[QASRLGenerationPrompt[SID], List[VerbQA]](
  helper, numAssignmentsForPrompt, initNumHITsToKeepActive, _promptSource
) with StrictLogging {

  import helper._
  import config._
  import QASRLSettings._
  import taskSpec.hitTypeId

  override def promptFinished(prompt: QASRLGenerationPrompt[SID]): Unit = {
    // sentenceTrackingActor ! GenerationFinished(prompt)
  }

  val badSentenceIdsFilename = "badSentenceIds"

  var badSentences = annotationDataService.loadLiveData(badSentenceIdsFilename)
    .map(_.mkString)
    .map(read[Set[SID]])
    .toOption.foldK

  private[this] def flagBadSentence(id: SID) = {
    badSentences = badSentences + id
    save
    for {
      (prompt, hitInfos) <- activeHITInfosByPromptIterator
      if prompt.id == id
      HITInfo(hit, _) <- hitInfos
    } yield helper.expireHIT(hit)
  }

  val coverageStatsFilename = "coverageStats"

  var coverageStats: Map[String, List[Int]] = annotationDataService.loadLiveData(coverageStatsFilename)
    .map(_.mkString)
    .map(read[Map[String, List[Int]]])
    .toOption.getOrElse(Map.empty[String, List[Int]])

  val feedbackFilename = "genFeedback"

  var feedbacks =
    annotationDataService.loadLiveData(feedbackFilename)
      .map(_.mkString)
      .map(read[List[Assignment[List[VerbQA]]]])
      .toOption.foldK

  private[this] def save = {
    annotationDataService.saveLiveData(
      coverageStatsFilename,
      write(coverageStats))
    annotationDataService.saveLiveData(
      feedbackFilename,
      write(feedbacks))
    annotationDataService.saveLiveData(
      badSentenceIdsFilename,
      write(badSentences))
    logger.info("Generation data saved.")
  }

  override def reviewAssignment(hit: HIT[QASRLGenerationPrompt[SID]], assignment: Assignment[List[VerbQA]]): Unit = {
    evaluateAssignment(hit, startReviewing(assignment), Approval(""))
    if(!assignment.feedback.isEmpty) {
      feedbacks = assignment :: feedbacks
      logger.info(s"Feedback: ${assignment.feedback}")
    }

    val newQuestionRecord = assignment.response.size :: coverageStats.get(assignment.workerId).foldK
    coverageStats = coverageStats.updated(assignment.workerId, newQuestionRecord)
    val verbsCompleted = newQuestionRecord.size
    val questionsPerVerb = newQuestionRecord.sum.toDouble / verbsCompleted
    if(questionsPerVerb < QASRLSettings.generationCoverageQuestionsPerVerbThreshold &&
         verbsCompleted > generationCoverageGracePeriod) {
      config.service.associateQualificationWithWorker(
        new AssociateQualificationWithWorkerRequest()
          .withQualificationTypeId(coverageDisqualificationTypeId)
          .withWorkerId(assignment.workerId)
          .withIntegerValue(1)
          .withSendNotification(true))
    }
    val validationPrompt = QASRLValidationPrompt(hit.prompt, hit.hitTypeId, hit.hitId, assignment.assignmentId, assignment.response)
    validationActor ! validationHelper.Message.AddPrompt(validationPrompt)
    // sentenceTrackingActor ! ValidationBegun(validationPrompt)
  }

  override lazy val receiveAux2: PartialFunction[Any, Unit] = {
    case SaveData => save
    case Pring => println("Generation manager pringed.")
    case fbs: FlagBadSentence[SID] => fbs match {
      case FlagBadSentence(id) => flagBadSentence(id)
    }
  }
}
