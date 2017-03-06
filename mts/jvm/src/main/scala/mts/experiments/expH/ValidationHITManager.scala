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
    initNumAssignmentsPerPrompt: Int,
    initNumHITsToKeepActive: Int) = {

    new ValidationHITManager(
      helper, generationActor, sentenceTrackingActor,
      initNumAssignmentsPerPrompt, initNumHITsToKeepActive,
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
  initNumAssignmentsPerPrompt: Int,
  initNumHITsToKeepActive: Int,
  _promptSource: Iterator[ValidationPrompt]
) extends NumAssignmentsHITManager[ValidationPrompt, List[ValidationAnswer]](
  helper, initNumAssignmentsPerPrompt, initNumHITsToKeepActive, _promptSource) {

  import ValidationHITManager._
  import helper._
  import config._
  import taskSpec.hitTypeId

  override lazy val receiveAux2: PartialFunction[Any, Unit] = {
    case Reflect => sender ! this // for debugging
    case SaveData => saveData
  }

  override def promptFinished(prompt: ValidationPrompt): Unit = {
    sentenceTrackingActor ! ValidationFinished(prompt)
    val assignments = promptToAssignments(prompt)
    val numValid = math.round(assignments.map(_.response.filter(_.isAnswer).size).mean - 0.01).toInt
    generationActor ! ValidationResult(prompt.genPrompt, prompt.sourceHITId, prompt.sourceAssignmentId, numValid)
    promptToAssignments = promptToAssignments - prompt
  }

  private[this] var allPrompts = loadPrompts

  override def addPrompt(prompt: ValidationPrompt): Unit = {
    super.addPrompt(prompt)
    allPrompts = prompt :: allPrompts
  }

  private[this] def numAgreed(
    a1: Assignment[List[ValidationAnswer]],
    a2: Assignment[List[ValidationAnswer]]
  ) = {
    a1.response.zip(a2.response).filter {
      case (InvalidQuestion, InvalidQuestion) => true
      case (Answer(span1), Answer(span2)) => !span1.intersect(span2).isEmpty
      // TODO: a better solution for agreement in the presence of redundancy.
      // for now, I'll just be nice.
      case (Redundant(_), Redundant(_)) => true
      case (Answer(_), Redundant(_)) => true
      case (Redundant(_), Answer(_)) => true
      case _ => false
    }.size
  }

  // keep track of worker accuracy
  case class WorkerInfo(
    workerId: String,
    numAssignmentsCompleted: Int,
    numComparisonInstances: Int,
    numComparisonAgreements: Int,
    warnedAt: Option[Int],
    blockedAt: Option[Int]) {

    def agreement = numComparisonAgreements.toDouble / numComparisonInstances

    def addAssignment = this.copy(
      numAssignmentsCompleted = this.numAssignmentsCompleted + 1
    )

    def addComparison(numTotal: Int, numAgreed: Int) = this.copy(
      numComparisonInstances = this.numComparisonInstances + numTotal,
      numComparisonAgreements = this.numComparisonAgreements + numAgreed
    )

    def warned = this.copy(warnedAt = Some(numAssignmentsCompleted))
    def blocked = this.copy(blockedAt = Some(numAssignmentsCompleted))

    def tryWarnOrBlock: WorkerInfo = blockedAt match {
      case Some(_) => this // already blocked
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

          } else this
        case Some(numWhenWarned) => // decide whether to block
          if(agreement < validationAgreementThreshold &&
               numAssignmentsCompleted - numWhenWarned >= validationBufferBeforeBlocking) {

            service.blockWorker(
              workerId,
              f"Agreement with other annotators failed to improve above ${agreement}%.2f after warning.")

            blocked

          } else this
      }
    }
  }

  object WorkerInfo {
    def empty(workerId: String) = WorkerInfo(workerId, 0, 0, 0, None, None)
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
  }

  private[this] var allWorkerInfo = {
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
      // TODO assemble from saved data?
      Map.empty[ValidationPrompt, List[Assignment[List[ValidationAnswer]]]]
    }
  }

  // override for more interesting review policy
  override def reviewAssignment(hit: HIT[ValidationPrompt], assignment: Assignment[List[ValidationAnswer]]): Unit = {
    evaluateAssignment(startReviewing(assignment), Approval(""))

    import assignment.workerId

    // grant bonus as appropriate
    val numQuestions = hit.prompt.qaPairs.size
    val totalBonus = validationBonusPerQuestion * (numQuestions - 4)
    if(totalBonus > 0.0) {
      service.grantBonus(
        workerId, totalBonus, assignment.assignmentId,
        s"Bonus of ${dollarsToCents(totalBonus)} awarded for validating $numQuestions questions."
      )
    }

    var newWorkerInfo = allWorkerInfo
      .get(workerId)
      .getOrElse(WorkerInfo.empty(workerId))
      .addAssignment
    // do comparisons with other workers
    promptToAssignments.get(hit.prompt).getOrElse(Nil).foreach { otherAssignment =>
      val otherWorkerId = otherAssignment.workerId
      val nAgreed = numAgreed(assignment, otherAssignment)
      // update current worker with comparison
      newWorkerInfo = newWorkerInfo
        .addComparison(numQuestions, nAgreed)
      // update the other one and put back in data structure (warning/blocking if necessary)
      val otherWorkerInfo = allWorkerInfo(otherWorkerId)
        .addComparison(numQuestions, nAgreed)
        .tryWarnOrBlock
      allWorkerInfo = allWorkerInfo.updated(otherWorkerId, otherWorkerInfo)
    }
    // now try just once warning or blocking the current worker before adding everything in
    newWorkerInfo = newWorkerInfo.tryWarnOrBlock
    allWorkerInfo = allWorkerInfo.updated(workerId, newWorkerInfo)
    promptToAssignments = promptToAssignments.updated(
      hit.prompt,
      assignment :: promptToAssignments.get(hit.prompt).getOrElse(Nil))
  }
}
