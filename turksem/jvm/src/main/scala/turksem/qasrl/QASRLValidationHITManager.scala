package turksem.qasrl

import turksem._
import turksem.util._
import turksem.qamr.GenerationPrompt
import turksem.qamr.Pring
import turksem.qamr.SaveData

import turkey._
import turkey.tasks._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.mturk.requester.AssignmentStatus
import com.amazonaws.mturk.requester.HITStatus

import upickle.default._

import com.typesafe.scalalogging.StrictLogging

object QASRLValidationHITManager {
  def apply[SID : Reader : Writer](
    helper: HITManager.Helper[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]],
    valQualificationTypeId: String,
    accuracyStatsActor: ActorRef,
    // sentenceTrackingActor: ActorRef,
    numAssignmentsForPrompt: QASRLValidationPrompt[SID] => Int,
    initNumHITsToKeepActive: Int)(
    implicit annotationDataService: AnnotationDataService
  ) = {

    new QASRLValidationHITManager[SID](
      helper, valQualificationTypeId, accuracyStatsActor, /*sentenceTrackingActor,*/
      numAssignmentsForPrompt, initNumHITsToKeepActive,
      loadPrompts[SID].iterator)
  }

  val validationPromptsFilename = "validationPrompts"

  def loadPrompts[SID : Reader](
    implicit annotationDataService: AnnotationDataService
  ) = annotationDataService.loadLiveData(validationPromptsFilename)
    .toOption
    .fold(List.empty[QASRLValidationPrompt[SID]])(lines => read[List[QASRLValidationPrompt[SID]]](lines.mkString))

  def notificationEmailText(curAgreement: Double): String = {
    import QASRLSettings._
    val explanatoryText = if(curAgreement < validationAgreementBlockingThreshold) {
      s"""There will be a grace period of several more assignments (${validationBufferBeforeBlocking} more after this calculation was done), and after that, if your agreement rate remains below ${math.round(validationAgreementBlockingThreshold * 100).toInt}%, you will no longer qualify for the task. Note that your qualification value may not accurately reflect your agreement rate: it will be prevented from going below ${math.round(validationAgreementBlockingThreshold * 100).toInt} until the grace period is over."""
    } else {
      s"""If this drops below ${math.round(validationAgreementBlockingThreshold * 100).toInt}%, you will no longer qualify for the task. There will be a grace period (${validationBufferBeforeBlocking} more assignments after this calculation was done) during which your qualification value will be prevented from dropping below ${math.round(validationAgreementBlockingThreshold * 100).toInt}."""
    }
    val dropOrRemain = if(curAgreement < validationAgreementBlockingThreshold) "remain" else "drop"

    f"""
The answer judgments that you have provided so far agree with other annotators ${math.round(curAgreement * 10000.0) / 100.0}%.2f%% of the time. $explanatoryText%s

If you are not sure why your score is this low, we recommend reading over the examples in the instructions again. We are still iterating on the task design and we are not yet sure what the common sources of disagreement are. After this batch of HITs, we will reset everyone's qualification values and try again with changes to the instructions and interface.

""".trim
  }
}

class QASRLValidationHITManager[SID : Reader : Writer] private (
  helper: HITManager.Helper[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]],
  valQualificationTypeId: String,
  accuracyStatsActor: ActorRef,
  // sentenceTrackingActor: ActorRef,
  numAssignmentsForPrompt: QASRLValidationPrompt[SID] => Int,
  initNumHITsToKeepActive: Int,
  _promptSource: Iterator[QASRLValidationPrompt[SID]])(
  implicit annotationDataService: AnnotationDataService
) extends NumAssignmentsHITManager[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]](
  helper, numAssignmentsForPrompt, initNumHITsToKeepActive, _promptSource) {

  import QASRLValidationHITManager._
  import helper._
  import config._
  import taskSpec.hitTypeId
  import QASRLSettings._

  override lazy val receiveAux2: PartialFunction[Any, Unit] = {
    case SaveData => save
    case Pring => println("Validation manager pringed.")
  }

  override def promptFinished(prompt: QASRLValidationPrompt[SID]): Unit = {
    val assignments = promptToAssignments(prompt)
    // sentenceTrackingActor ! ValidationFinished(prompt, assignments)
    val numValid = QASRLValidationAnswer.numValidQuestions(assignments.map(_.response))
    accuracyStatsActor ! QASRLValidationResult(prompt.genPrompt, prompt.sourceHITTypeId, prompt.sourceHITId, prompt.sourceAssignmentId, numValid)
    promptToAssignments = promptToAssignments - prompt
  }

  private[this] var allPrompts = loadPrompts[SID]

  override def addPrompt(prompt: QASRLValidationPrompt[SID]): Unit = {
    super.addPrompt(prompt)
    allPrompts = prompt :: allPrompts
  }

  val workerInfoFilename = "validationWorkerInfo"
  val promptToAssignmentsFilename = "promptToAssignments"

  private[this] def save = {
    annotationDataService.saveLiveData(
      workerInfoFilename,
      write[Map[String, QASRLValidationWorkerInfo]](allWorkerInfo))
    annotationDataService.saveLiveData(
      promptToAssignmentsFilename,
      write[Map[QASRLValidationPrompt[SID], List[Assignment[List[QASRLValidationAnswer]]]]](promptToAssignments))
    annotationDataService.saveLiveData(
      validationPromptsFilename,
      write[List[QASRLValidationPrompt[SID]]](allPrompts))
    annotationDataService.saveLiveData(
      feedbackFilename,
      write[List[Assignment[List[QASRLValidationAnswer]]]](feedbacks))
    logger.info("Validation data saved.")
  }

  var allWorkerInfo = {
    annotationDataService.loadLiveData(workerInfoFilename)
      .map(_.mkString)
      .map(read[Map[String, QASRLValidationWorkerInfo]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      Map.empty[String, QASRLValidationWorkerInfo]
    }
  }

  private[this] var promptToAssignments = {
    annotationDataService.loadLiveData(promptToAssignmentsFilename)
      .map(_.mkString)
      .map(read[Map[QASRLValidationPrompt[SID], List[Assignment[List[QASRLValidationAnswer]]]]])
      .toOption.getOrElse {
      Map.empty[QASRLValidationPrompt[SID], List[Assignment[List[QASRLValidationAnswer]]]]
    }
  }

  val feedbackFilename = "valFeedback"

  var feedbacks =
    annotationDataService.loadLiveData(feedbackFilename)
      .map(_.mkString)
      .map(read[List[Assignment[List[QASRLValidationAnswer]]]])
      .toOption.getOrElse {
      List.empty[Assignment[List[QASRLValidationAnswer]]]
    }

  def tryWarnOrBlock(worker: QASRLValidationWorkerInfo): QASRLValidationWorkerInfo = {
    if(worker.agreement.isNaN) worker // this means no comparisons have been done yet
    else worker.warnedAt match {
      case None =>
        // set soft qualification since no warning yet
        config.service.updateQualificationScore(
          valQualificationTypeId,
          worker.workerId,
          math.ceil(100 * math.max(worker.agreement, validationAgreementBlockingThreshold)).toInt)
        if(worker.agreement < validationAgreementWarningThreshold &&
             worker.numAssignmentsCompleted >= validationBufferBeforeWarning) {

          service.notifyWorkers(
            "Notification (warning + tips) regarding the question answering task",
            notificationEmailText(worker.agreement),
            Array(worker.workerId))

          logger.info(s"Validation worker ${worker.workerId} warned at ${worker.numAssignmentsCompleted} with accuracy ${worker.agreement}")
          worker.warned

        } else worker
      case Some(numWhenWarned) =>
        if(worker.numAssignmentsCompleted - numWhenWarned >= validationBufferBeforeBlocking) {
          config.service.updateQualificationScore(
            valQualificationTypeId,
            worker.workerId,
            math.ceil(100 * worker.agreement).toInt)
          if(math.ceil(worker.agreement).toInt < validationAgreementBlockingThreshold) {

            logger.info(s"Validation worker ${worker.workerId} DQ'd at ${worker.numAssignmentsCompleted} with accuracy ${worker.agreement}")
            worker.blocked
          } else worker
        } else {
          // set soft qualification since still in buffer zone
          config.service.updateQualificationScore(
            valQualificationTypeId,
            worker.workerId,
            math.ceil(100 * math.max(worker.agreement, validationAgreementBlockingThreshold)).toInt)
          worker
        }
    }
  }

  // override for more interesting review policy
  override def reviewAssignment(hit: HIT[QASRLValidationPrompt[SID]], assignment: Assignment[List[QASRLValidationAnswer]]): Unit = {
    evaluateAssignment(hit, startReviewing(assignment), Approval(""))
    if(!assignment.feedback.isEmpty) {
      feedbacks = assignment :: feedbacks
      logger.info(s"Feedback: ${assignment.feedback}")
    }

    import assignment.workerId

    // grant bonus as appropriate
    val numQuestions = hit.prompt.qaPairs.size
    val totalBonus = validationBonus(numQuestions)
    if(totalBonus > 0.0) {
      service.grantBonus(
        workerId, totalBonus, assignment.assignmentId,
        s"Bonus of ${dollarsToCents(totalBonus)}c awarded for validating $numQuestions questions."
      )
    }

    var newWorkerInfo = allWorkerInfo
      .get(workerId)
      .getOrElse(QASRLValidationWorkerInfo.empty(workerId))
      .addAssignment(assignment.response,
                     assignment.submitTime - assignment.acceptTime,
                     taskSpec.hitType.reward + totalBonus)
    // do comparisons with other workers
    promptToAssignments.get(hit.prompt).getOrElse(Nil).foreach { otherAssignment =>
      val otherWorkerId = otherAssignment.workerId
      val nAgreed = QASRLValidationAnswer.numAgreed(assignment.response, otherAssignment.response)
      // update current worker with comparison
      newWorkerInfo = newWorkerInfo
        .addComparison(numQuestions, nAgreed)
      // update the other one and put back in data structure (warning/blocking if necessary)
      val otherWorkerInfo = tryWarnOrBlock(
        allWorkerInfo(otherWorkerId).addComparison(numQuestions, nAgreed)
      )
      allWorkerInfo = allWorkerInfo.updated(otherWorkerId, otherWorkerInfo)
    }
    // now try just once warning or blocking the current worker before adding everything in
    newWorkerInfo = tryWarnOrBlock(newWorkerInfo)
    allWorkerInfo = allWorkerInfo.updated(workerId, newWorkerInfo)
    promptToAssignments = promptToAssignments.updated(
      hit.prompt,
      assignment :: promptToAssignments.get(hit.prompt).getOrElse(Nil))
  }
}
