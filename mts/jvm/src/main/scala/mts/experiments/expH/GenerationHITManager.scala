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

object GenerationHITManager {
  def notificationEmailText(curAccuracy: Double) = {
    val explanatoryText = if(curAccuracy < generationAccuracyBlockingThreshold) {
      s"""There will be a grace period of several more assignments, and after that, if your accuracy remains below ${math.round(generationAccuracyBlockingThreshold * 100).toInt}%, you will be blocked."""
    } else {
      s"""You are fine for now, but if this drops below ${math.round(generationAccuracyBlockingThreshold * 100).toInt}%, you will be blocked."""
    }
    val dropOrRemain = if(curAccuracy < generationAccuracyBlockingThreshold) "remain" else "drop"
    f"""
Of your question-answer pairs that have been reviewed so far, ${math.round(curAccuracy * 10000.0) / 100.0}%.2f%% were judged valid or non-redundant by validators. $explanatoryText%s If you are having trouble writing grammatical questions for all of the words you are given, keep a few things in mind:

  1) You can use a special word in either the question or the answer. Sometimes it is hard to form a nice question-answer pair one way, but it is very easy to do it the other way.
  2) The answer can contain more than just the special word. Especially with proper names that contain several words, you may be able to use that full name as the answer to a few questions, and spread those question-answer pairs over the set of special words you were given.

Also be sure not to write any redundant questions. Before you continue, we suggest that you carefully read over the instructions again to maximize the rewards you can get out of the task.

Finally, it is always possible that you got unlucky. If your responses are high-quality, then your accuracy will likely not $dropOrRemain%s too low. However, because this process is inherently random, we cannot guarantee that no high-quality workers will be blocked.
""".trim
  }
}

class GenerationHITManager(
  helper: HITManager.Helper[GenerationPrompt, List[WordedQAPair]],
  validationHelper: HITManager.Helper[ValidationPrompt, List[ValidationAnswer]],
  validationActor: ActorRef,
  sentenceTrackingActor: ActorRef,
  numAssignmentsForPrompt: GenerationPrompt => Int,
  initNumHITsToKeepActive: Int,
  _promptSource: Iterator[GenerationPrompt]
) extends NumAssignmentsHITManager[GenerationPrompt, List[WordedQAPair]](
  helper, numAssignmentsForPrompt, initNumHITsToKeepActive, _promptSource) {

  import GenerationHITManager._
  import helper._
  import config._
  import taskSpec.hitTypeId

  override def promptFinished(prompt: GenerationPrompt): Unit = {
    sentenceTrackingActor ! GenerationFinished(prompt)
  }

  val workerStatsFilename = "generationWorkerStats"

  var allWorkerStats =
    FileManager.loadDataFile(finalExperimentName, workerStatsFilename)
      .map(_.mkString)
      .map(read[Map[String, WorkerStats]])
      .toOption.getOrElse {
      // TODO assemble from saved data
      Map.empty[String, WorkerStats]
    }

  val feedbackFilename = "genFeedback"

  var feedbacks =
    FileManager.loadDataFile(finalExperimentName, feedbackFilename)
      .map(_.mkString)
      .map(read[List[Assignment[List[WordedQAPair]]]])
      .toOption.getOrElse {
      List.empty[Assignment[List[WordedQAPair]]]
    }

  private[this] def saveData = {
    FileManager.saveDataFile(
      finalExperimentName,
      workerStatsFilename,
      write[Map[String, WorkerStats]](allWorkerStats))
    FileManager.saveDataFile(
      finalExperimentName,
      feedbackFilename,
      write[List[Assignment[List[WordedQAPair]]]](feedbacks))
  }

  override def reviewAssignment(hit: HIT[GenerationPrompt], assignment: Assignment[List[WordedQAPair]]): Unit = {
    evaluateAssignment(hit, startReviewing(assignment), Approval(""))
    if(!assignment.feedback.isEmpty) {
      feedbacks = assignment :: feedbacks
      println(s"Feedback: ${assignment.feedback}")
    }
    val validationPrompt = ValidationPrompt(hit.prompt, hit.hitId, assignment.assignmentId, assignment.response)
    validationActor ! validationHelper.Message.AddPrompt(validationPrompt)
    sentenceTrackingActor ! ValidationBegun(validationPrompt)
  }

  override lazy val receiveAux2: PartialFunction[Any, Unit] = {
    case SaveData => saveData
    case ValidationResult(prompt, hitId, assignmentId, numQAsValid) =>
      val hit = FileManager.getHIT[GenerationPrompt](hitTypeId, hitId).get
      val assignment = FileManager.loadAssignmentsForHIT[List[WordedQAPair]](hitTypeId, hitId)
        .find(_.assignmentId == assignmentId).get

      // award bonuses
      val numSpecialWords = prompt.keywords.size
      val numQAsProvided = assignment.response.size
      val bonusAwarded = generationBonus(numSpecialWords, numQAsValid)
      if(bonusAwarded > 0.0) {
        service.grantBonus(
          assignment.workerId, bonusAwarded, assignment.assignmentId,
          s"""$numQAsValid out of $numQAsProvided question-answer pairs were judged to be valid,
            where at least $numSpecialWords were required, for a bonus of
            ${dollarsToCents(bonusAwarded)}c.""")
      }

      val stats = allWorkerStats
        .get(assignment.workerId)
        .getOrElse(WorkerStats.empty(assignment.workerId))
        .addAssignment(assignment.response.size, numQAsValid,
                       assignment.submitTime - assignment.acceptTime,
                       taskSpec.hitType.reward + bonusAwarded)

      // warn or block worker if performance is unsatisfactory
      val newStats = stats.blockedAt match {
        case Some(_) => stats // already blocked
        case None => stats.warnedAt match {
          case None => // decide whether to warn
            if(stats.accuracy < generationAccuracyWarningThreshold &&
                 stats.numAssignmentsCompleted >= generationBufferBeforeWarning) {

              service.notifyWorkers(
                "Notification (warning + tips) regarding the question-answer task",
                notificationEmailText(stats.accuracy),
                Array(assignment.workerId)
              )

              stats.warned

            } else stats
          case Some(numWhenWarned) => // decide whether to block
            if(stats.accuracy < generationAccuracyBlockingThreshold &&
                 stats.numAssignmentsCompleted - numWhenWarned >= generationBufferBeforeBlocking) {

              service.blockWorker(
                assignment.workerId,
                f"Accuracy failed to remain above ${generationAccuracyBlockingThreshold}%.2f.")

              stats.blocked

            } else stats
        }
      }

      allWorkerStats = allWorkerStats.updated(assignment.workerId, newStats)
  }
}
