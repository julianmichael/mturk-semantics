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
    valQualificationTypeId: String,
    generationActor: ActorRef,
    sentenceTrackingActor: ActorRef,
    numAssignmentsForPrompt: ValidationPrompt => Int,
    initNumHITsToKeepActive: Int) = {

    new ValidationHITManager(
      helper, valQualificationTypeId, generationActor, sentenceTrackingActor,
      numAssignmentsForPrompt, initNumHITsToKeepActive,
      loadPrompts.iterator)
  }

  val validationPromptsFilename = "validationPrompts"

  def loadPrompts = FileManager.loadDataFile(finalExperimentName, validationPromptsFilename)
    .toOption
    .fold(List.empty[ValidationPrompt])(lines => read[List[ValidationPrompt]](lines.mkString))

  def notificationEmailText(curAgreement: Double): String = {
    val explanatoryText = if(curAgreement < validationAgreementBlockingThreshold) {
      s"""There will be a grace period of several more assignments ($validationBufferBeforeBlocking more after this calculation was done), and after that, if your agreement rate remains below ${math.round(validationAgreementBlockingThreshold * 100).toInt}%, you will no longer qualify for the task. Note that your qualification value may not be accurate during this grace period: it will be prevented from going below ${math.round(validationAgreementBlockingThreshold * 100).toInt} until the grace period is over."""
    } else {
      s"""You are fine for now, but if this drops below ${math.round(validationAgreementBlockingThreshold * 100).toInt}%, you will no longer qualify for the task. There will be a grace period ($validationBufferBeforeBlocking more assignments after this calculation was done) during which your qualification value will be prevented from dropping below ${math.round(validationAgreementBlockingThreshold * 100).toInt}."""
    }
    val dropOrRemain = if(curAgreement < validationAgreementBlockingThreshold) "remain" else "drop"
    f"""
The answer judgments that you have provided so far agree with other annotators ${math.round(curAgreement * 10000.0) / 100.0}%.2f%% of the time. $explanatoryText%s

Common reasons for disagreement may include:

  1) Grammaticality. Be sure to mark ungrammatical questions as invalid. Since this is not always clear-cut, try to adjust your standards so that on average, you count about 10%% to 15%% of questions as invalid. (Of course, since it varies by who wrote them, some groups of questions will be worse than others.)
  2) Other rules for validity. Be sure to mark yes/no or either/or questions as invalid, as well as questions which are explicitly about the words in the sentence rather than about the meaning of the sentence.
  3) Redundancy. Remember to mark redundant questions. (Though, don't mark them as redundant just because they have the same answer—see the task instructions for details.)

Before continuing to work on the task, we suggest that you carefully review the instructions and make sure you understand the requirements.

Finally, it is always possible that you got unlucky and were compared to low-quality workers who have not been filtered out yet. If your responses are high-quality, then your agreement rates will likely not $dropOrRemain%s too low and you will be fine. However, because this process is inherently random, we cannot guarantee that no high-quality workers not lose their qualification.
""".trim
  }
}

class ValidationHITManager private (
  helper: HITManager.Helper[ValidationPrompt, List[ValidationAnswer]],
  valQualificationTypeId: String,
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
    println("Validation data saved.")
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
    worker.warnedAt match {
      case None =>
        // set soft qualification since no warning yet
        config.service.updateQualificationScore(
          valQualificationTypeId,
          worker.workerId,
          math.ceil(math.max(worker.agreement, validationAgreementBlockingThreshold)).toInt)
        if(worker.agreement < validationAgreementWarningThreshold &&
             worker.numAssignmentsCompleted >= validationBufferBeforeWarning) {

          service.notifyWorkers(
            "Notification (warning + tips) regarding the question answering task",
            notificationEmailText(worker.agreement),
            Array(worker.workerId))

          worker.warned

        } else worker
      case Some(numWhenWarned) =>
        if(worker.numAssignmentsCompleted - numWhenWarned >= validationBufferBeforeBlocking) {
          config.service.updateQualificationScore(
            valQualificationTypeId,
            worker.workerId,
            math.ceil(worker.agreement).toInt)
          if(math.ceil(worker.agreement).toInt < validationAgreementBlockingThreshold) {
            worker.blocked
          } else worker
        } else {
          // set soft qualification since still in buffer zone
          config.service.updateQualificationScore(
            valQualificationTypeId,
            worker.workerId,
            math.ceil(math.max(worker.agreement, validationAgreementBlockingThreshold)).toInt)
          worker
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
