package mts.experiments.expH

import mts.tasks._
import mts.core._
import mts.util._
import mts.experiments._

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.ActorRef

import com.amazonaws.mturk.requester.AssignmentStatus
import com.amazonaws.mturk.requester.HITStatus

import upickle.default._

object ValidationHITManager {
  def apply(
    helper: HITManager.Helper[ValidationPrompt, List[ValidationAnswer]],
    generationActor: ActorRef,
    sentenceTrackingActor: ActorRef,
    numAssignmentsForPrompt: ValidationPrompt => Int,
    initNumHITsToKeepActive: Int) = {

    new ValidationHITManager(
      helper, generationActor, sentenceTrackingActor,
      numAssignmentsForPrompt, initNumHITsToKeepActive,
      loadPrompts.iterator)
  }

  val validationPromptsFilename = "validationPrompts"

  def loadPrompts = FileManager.loadDataFile(finalExperimentName, validationPromptsFilename)
    .toOption
    .fold(List.empty[ValidationPrompt])(lines => read[List[ValidationPrompt]](lines.mkString))
}

class ValidationHITManager private (
  helper: HITManager.Helper[ValidationPrompt, List[ValidationAnswer]],
  generationActor: ActorRef,
  sentenceTrackingActor: ActorRef,
  numAssignmentsForPrompt: ValidationPrompt => Int,
  initNumHITsToKeepActive: Int,
  _promptSource: Iterator[ValidationPrompt]
) extends NumAssignmentsHITManager[ValidationPrompt, List[ValidationAnswer]](
  helper, numAssignmentsForPrompt, initNumHITsToKeepActive, _promptSource) {

  import ValidationHITManager._
  import helper._
  import config._
  import taskSpec.hitTypeId

  override lazy val receiveAux2: PartialFunction[Any, Unit] = {
    case SaveData => saveData
  }

  override def promptFinished(prompt: ValidationPrompt): Unit = {
    val assignments = promptToAssignments(prompt)
    sentenceTrackingActor ! ValidationFinished(prompt, assignments)
    val numValid = numValidQuestions(assignments.map(_.response))
    generationActor ! ValidationResult(prompt.genPrompt, prompt.sourceHITId, prompt.sourceAssignmentId, numValid)
    promptToAssignments = promptToAssignments - prompt
  }

  private[this] var allPrompts = loadPrompts

  override def addPrompt(prompt: ValidationPrompt): Unit = {
    super.addPrompt(prompt)
    allPrompts = prompt :: allPrompts
  }

  val workerInfoFilename = "validationWorkerInfo"
  val promptToAssignmentsFilename = "promptToAssignments"

  private[this] def saveData = {
    FileManager.saveDataFile(
      finalExperimentName,
      workerInfoFilename,
      write[Map[String, WorkerInfo]](allWorkerInfo))
    FileManager.saveDataFile(
      finalExperimentName,
      promptToAssignmentsFilename,
      write[Map[ValidationPrompt, List[Assignment[List[ValidationAnswer]]]]](promptToAssignments))
    FileManager.saveDataFile(
      finalExperimentName,
      validationPromptsFilename,
      write[List[ValidationPrompt]](allPrompts))
    FileManager.saveDataFile(
      finalExperimentName,
      feedbackFilename,
      write[List[Assignment[List[ValidationAnswer]]]](feedbacks))
  }

  var allWorkerInfo = {
    FileManager.loadDataFile(finalExperimentName, workerInfoFilename)
      .map(_.mkString)
      .map(read[Map[String, WorkerInfo]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      Map.empty[String, WorkerInfo]
    }
  }

  private[this] var promptToAssignments = {
    FileManager.loadDataFile(finalExperimentName, promptToAssignmentsFilename)
      .map(_.mkString)
      .map(read[Map[ValidationPrompt, List[Assignment[List[ValidationAnswer]]]]])
      .toOption.getOrElse {
      Map.empty[ValidationPrompt, List[Assignment[List[ValidationAnswer]]]]
    }
  }

  val feedbackFilename = "valFeedback"

  var feedbacks =
    FileManager.loadDataFile(finalExperimentName, feedbackFilename)
      .map(_.mkString)
      .map(read[List[Assignment[List[ValidationAnswer]]]])
      .toOption.getOrElse {
      List.empty[Assignment[List[ValidationAnswer]]]
    }

  def tryWarnOrBlock(worker: WorkerInfo): WorkerInfo = {
    import worker._
    blockedAt match {
      case Some(_) => worker // already blocked
      case None => warnedAt match {
        case None => // decide whether to warn
          if(agreement < validationAgreementThreshold &&
               numAssignmentsCompleted >= validationBufferBeforeWarning) {

            service.notifyWorkers(
              "Warning: your performance on the question answering task",
              s"""The answer judgments that you have provided so far agree with other annotators less than
                  ${math.round(validationAgreementThreshold * 100).toInt}% of the time.
                  We suggest you stop working on the task;
                  if you continue and your performance does not improve, you will be blocked.""",
              Array(workerId)
            )

            warned

          } else worker
        case Some(numWhenWarned) => // decide whether to block
          if(agreement < validationAgreementThreshold &&
               numAssignmentsCompleted - numWhenWarned >= validationBufferBeforeBlocking) {

            service.blockWorker(
              workerId,
              f"Agreement with other annotators failed to improve above ${agreement}%.2f after warning.")

            blocked

          } else worker
      }
    }
  }

  // override for more interesting review policy
  override def reviewAssignment(hit: HIT[ValidationPrompt], assignment: Assignment[List[ValidationAnswer]]): Unit = {
    evaluateAssignment(hit, startReviewing(assignment), Approval(""))
    if(!assignment.feedback.isEmpty) {
      feedbacks = assignment :: feedbacks
      println(s"Feedback: ${assignment.feedback}")
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
      .getOrElse(WorkerInfo.empty(workerId))
      .addAssignment(assignment.response,
                     assignment.submitTime - assignment.acceptTime,
                     taskSpec.hitType.reward + totalBonus)
    // do comparisons with other workers
    promptToAssignments.get(hit.prompt).getOrElse(Nil).foreach { otherAssignment =>
      val otherWorkerId = otherAssignment.workerId
      val nAgreed = numAgreed(assignment, otherAssignment)
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
