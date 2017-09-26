package turksem.qasrl

import turksem._
import turksem.util._
import turksem.qamr.Pring
import turksem.qamr.SaveData

import turkey._
import turkey.tasks._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.services.mturk.model.AssociateQualificationWithWorkerRequest
import com.amazonaws.services.mturk.model.SendBonusRequest
import com.amazonaws.services.mturk.model.NotifyWorkersRequest
import com.amazonaws.services.mturk.model.CreateWorkerBlockRequest
import com.amazonaws.services.mturk.model.ListWorkersWithQualificationTypeRequest
import com.amazonaws.services.mturk.model.DisassociateQualificationFromWorkerRequest

import upickle.default._

import com.typesafe.scalalogging.StrictLogging

class QASRLValidationHITManager[SID : Reader : Writer](
  helper: HITManager.Helper[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]],
  valDisqualificationTypeId: String,
  accuracyStatsActor: ActorRef,
  // sentenceTrackingActor: ActorRef,
  numAssignmentsForPrompt: QASRLValidationPrompt[SID] => Int,
  initNumHITsToKeepActive: Int)(
  implicit annotationDataService: AnnotationDataService
) extends NumAssignmentsHITManager[QASRLValidationPrompt[SID], List[QASRLValidationAnswer]](
  helper, numAssignmentsForPrompt, initNumHITsToKeepActive, List.empty[QASRLValidationPrompt[SID]].iterator) {

  override lazy val receiveAux2: PartialFunction[Any, Unit] = {
    case SaveData => save
    case Pring => println("Validation manager pringed.")
    case ChristenWorker(workerId, numAgreementsToAdd) => christenWorker(workerId, numAgreementsToAdd)
  }

  override def promptFinished(prompt: QASRLValidationPrompt[SID]): Unit = {
    val assignments = promptToAssignments(prompt)
    // sentenceTrackingActor ! ValidationFinished(prompt, assignments)
    val numValid = QASRLValidationAnswer.numValidQuestions(assignments.map(_.response))
    accuracyStatsActor ! QASRLValidationResult(prompt.genPrompt, prompt.sourceHITTypeId, prompt.sourceHITId, prompt.sourceAssignmentId, numValid)
    promptToAssignments = promptToAssignments - prompt
  }

  override def addPrompt(prompt: QASRLValidationPrompt[SID]): Unit = {
    super.addPrompt(prompt)
    allPrompts = prompt :: allPrompts
  }

  def christenWorker(workerId: String, numAgreementsToAdd: Int) = {
    allWorkerInfo = allWorkerInfo.get(workerId).fold(allWorkerInfo) { info =>
      allWorkerInfo.updated(workerId, info.addBonusAgreements(numAgreementsToAdd))
    }
  }

  val validationPromptsFilename = "validationPrompts"
  val workerInfoFilename = "validationWorkerInfo"
  val promptToAssignmentsFilename = "promptToAssignments"

  private[this] var allPrompts = {
    val prompts = annotationDataService.loadLiveData(validationPromptsFilename)
      .toOption
      .fold(List.empty[QASRLValidationPrompt[SID]])(lines => read[List[QASRLValidationPrompt[SID]]](lines.mkString))
    prompts.reverse.foreach(super.addPrompt) // add them back while loading
    prompts
  }

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

  import scala.collection.JavaConverters._

  def assessQualification(worker: QASRLValidationWorkerInfo): Unit = {
    for {
      workerIsDisqualified <- Try(
        helper.config.service.listWorkersWithQualificationType(
          new ListWorkersWithQualificationTypeRequest()
            .withQualificationTypeId(valDisqualificationTypeId)
        ).getQualifications.asScala.toList.map(_.getWorkerId).contains(worker.workerId))
      workerShouldBeDisqualified = (
        !worker.agreement.isNaN &&
          worker.agreement < QASRLSettings.validationAgreementBlockingThreshold &&
          worker.numAssignmentsCompleted > QASRLSettings.validationAgreementGracePeriod)
      _ <- Try(
        if(workerIsDisqualified && !workerShouldBeDisqualified) {
          helper.config.service.disassociateQualificationFromWorker(
            new DisassociateQualificationFromWorkerRequest()
              .withQualificationTypeId(valDisqualificationTypeId)
              .withWorkerId(worker.workerId))
        } else if(!workerIsDisqualified && workerShouldBeDisqualified) {
          helper.config.service.associateQualificationWithWorker(
            new AssociateQualificationWithWorkerRequest()
              .withQualificationTypeId(valDisqualificationTypeId)
              .withWorkerId(worker.workerId)
              .withIntegerValue(1)
              .withSendNotification(true))
        } else Success(()))
    } yield ()
  }

  // override for more interesting review policy
  override def reviewAssignment(hit: HIT[QASRLValidationPrompt[SID]], assignment: Assignment[List[QASRLValidationAnswer]]): Unit = {
    helper.evaluateAssignment(hit, helper.startReviewing(assignment), Approval(""))
    if(!assignment.feedback.isEmpty) {
      feedbacks = assignment :: feedbacks
      logger.info(s"Feedback: ${assignment.feedback}")
    }

    import assignment.workerId

    // grant bonus as appropriate
    val numQuestions = hit.prompt.qaPairs.size
    val totalBonus = QASRLSettings.validationBonus(numQuestions)
    if(totalBonus > 0.0) {
      helper.config.service.sendBonus(
        new SendBonusRequest()
          .withWorkerId(workerId)
          .withBonusAmount(f"$totalBonus%.2f")
          .withAssignmentId(assignment.assignmentId)
          .withReason(s"Bonus of ${dollarsToCents(totalBonus)}c awarded for validating $numQuestions questions.")
      )
    }

    var newWorkerInfo = allWorkerInfo
      .get(workerId)
      .getOrElse(QASRLValidationWorkerInfo.empty(workerId))
      .addAssignment(assignment.response,
                     assignment.submitTime - assignment.acceptTime,
                     helper.taskSpec.hitType.reward + totalBonus)

    if(newWorkerInfo.proportionInvalid > 0.7 && newWorkerInfo.numAssignmentsCompleted >= 10) {
      helper.config.service.createWorkerBlock(
        new CreateWorkerBlockRequest()
          .withWorkerId(newWorkerInfo.workerId)
          .withReason("You have been blocked for spamming the question answering task. If you believe this was in error, please contact the requester.")
      )
    }
    val proportionInvalid = newWorkerInfo.numInvalids.toDouble / (newWorkerInfo.numAnswerSpans + newWorkerInfo.numInvalids)
    // do comparisons with other workers
    promptToAssignments.get(hit.prompt).getOrElse(Nil).foreach { otherAssignment =>
      val otherWorkerId = otherAssignment.workerId
      val nAgreed = QASRLValidationAnswer.numAgreed(assignment.response, otherAssignment.response)
      // update current worker with comparison
      newWorkerInfo = newWorkerInfo
        .addComparison(numQuestions, nAgreed)
      // update the other one and put back in data structure (blocking if necessary)
      val otherWorkerInfo = allWorkerInfo(otherWorkerId).addComparison(numQuestions, nAgreed)
      assessQualification(otherWorkerInfo)
      allWorkerInfo = allWorkerInfo.updated(otherWorkerId, otherWorkerInfo)
    }
    // now blocking the current worker if necessary before adding everything in
    assessQualification(newWorkerInfo)
    allWorkerInfo = allWorkerInfo.updated(workerId, newWorkerInfo)
    promptToAssignments = promptToAssignments.updated(
      hit.prompt,
      assignment :: promptToAssignments.get(hit.prompt).getOrElse(Nil))
  }
}
