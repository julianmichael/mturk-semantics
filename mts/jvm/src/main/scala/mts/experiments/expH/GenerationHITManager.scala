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

class GenerationHITManager(
  helper: HITManager.Helper[GenerationPrompt, List[WordedQAPair]],
  validationHelper: HITManager.Helper[ValidationPrompt, List[ValidationAnswer]],
  validationActor: ActorRef,
  sentenceTrackingActor: ActorRef,
  initNumAssignmentsPerPrompt: Int,
  initNumHITsToKeepActive: Int,
  _promptSource: Iterator[GenerationPrompt]
) extends NumAssignmentsHITManager[GenerationPrompt, List[WordedQAPair]](
  helper, initNumAssignmentsPerPrompt, initNumHITsToKeepActive, _promptSource) {

  import helper._
  import config._
  import taskSpec.hitTypeId

  override def promptFinished(prompt: GenerationPrompt): Unit = {
    sentenceTrackingActor ! prompt
  }

  // keep track of worker accuracy
  case class WorkerStats(
    workerId: String,
    numAssignmentsCompleted: Int,
    numQAPairsWritten: Int,
    numQAPairsValid: Int,
    warnedAt: Option[Int],
    blockedAt: Option[Int]) {

    def accuracy = numQAPairsValid.toDouble / numQAPairsWritten

    def addAssignment(
      numWritten: Int,
      numValid: Int
    ) = WorkerStats(
      workerId,
      numAssignmentsCompleted + 1,
      numQAPairsWritten + numWritten,
      numQAPairsValid + numValid,
      warnedAt, blockedAt)

    def warned = this.copy(warnedAt = Some(numAssignmentsCompleted))
    def blocked = this.copy(blockedAt = Some(numAssignmentsCompleted))
  }
  object WorkerStats {
    def empty(workerId: String) = WorkerStats(workerId, 0, 0, 0, None, None)
  }

  val workerStatsFilename = "generationWorkerStats"

  private[this] def saveData = FileManager.saveDataFile(
    finalExperimentName,
    workerStatsFilename,
    write[Map[String, WorkerStats]](allWorkerStats))

  private[this] var allWorkerStats =
    FileManager.loadDataFile(finalExperimentName, workerStatsFilename)
      .map(_.mkString)
      .map(read[Map[String, WorkerStats]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      Map.empty[String, WorkerStats]
    }

  override def reviewAssignment(hit: HIT[GenerationPrompt], assignment: Assignment[List[WordedQAPair]]): Unit = {
    evaluateAssignment(startReviewing(assignment), Approval(""))
    val validationPrompt = ValidationPrompt(hit.prompt, hit.hitId, assignment.assignmentId, assignment.response)
    validationActor ! validationHelper.Message.AddPrompt(validationPrompt)
    sentenceTrackingActor ! ValidationBegun(validationPrompt)
  }

  override lazy val receiveAux2: PartialFunction[Any, Unit] = {
    case Reflect => sender ! this // for debugging
    case SaveData => saveData
    case ValidationResult(prompt, hitId, assignmentId, numQAsValid) =>
      val hit = FileManager.getHIT[GenerationPrompt](hitTypeId, hitId).get
      val assignment = FileManager.loadAssignmentsForHIT[List[WordedQAPair]](hitTypeId, hitId)
        .find(_.assignmentId == assignmentId).get
      val stats = allWorkerStats
        .get(assignment.workerId)
        .getOrElse(WorkerStats.empty(assignment.workerId))
        .addAssignment(assignment.response.size, numQAsValid)

      // award bonuses
      val numSpecialWords = prompt.keywords.size
      val numQAsProvided = assignment.response.size
      val bonusAwarded = (1 to (numQAsValid - numSpecialWords)).map(bonusFor).sum
      service.grantBonus(
        assignment.workerId, bonusAwarded, assignment.assignmentId,
        s"""$numQAsValid out of $numQAsProvided question-answer pairs were judged to be valid,
            where at least $numSpecialWords were required, for a bonus of
            ${dollarsToCents(bonusAwarded)}c.""")

      // warn or block worker if performance is unsatisfactory
      val newStats = stats.blockedAt match {
        case Some(_) => stats // already blocked
        case None => stats.warnedAt match {
          case None => // decide whether to warn
            if(stats.accuracy < generationAccuracyThreshold &&
                 stats.numAssignmentsCompleted >= generationBufferBeforeWarning) {

              service.notifyWorkers(
                "Warning: your performance on the question-answer task",
                s"""Of your question-answer pairs that have been reviewed so far,
                    fewer than ${math.round(generationAccuracyThreshold * 100).toInt}%
                    were judged valid by validators. We suggest you stop working on the task;
                    if you continue and your performance does not improve, you will be blocked.""",
                Array(assignment.workerId)
              )

              stats.warned

            } else stats
          case Some(numWhenWarned) => // decide whether to block
            if(stats.accuracy < generationAccuracyThreshold &&
                 stats.numAssignmentsCompleted - numWhenWarned >= generationBufferBeforeBlocking) {

              service.blockWorker(
                assignment.workerId,
                f"Accuracy failed to improve above ${stats.accuracy}%.2f after warning.")

              stats.blocked

            } else stats
        }
      }

      allWorkerStats = allWorkerStats.updated(assignment.workerId, newStats)
  }
}
