package turksem.qasrl

import turkey._
import turkey.tasks._

import turksem._
import turksem.util._
import turksem.qamr.GenerationPrompt
import turksem.qamr.WorkerStats
import turksem.qamr.Pring
import turksem.qamr.SaveData

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.{Actor, ActorRef}

import com.amazonaws.mturk.requester.AssignmentStatus
import com.amazonaws.mturk.requester.HITStatus

import upickle.default._

import com.typesafe.scalalogging.StrictLogging

class QASRLGenerationAccuracyManager[SID : Reader : Writer](
  genQualificationTypeId: String)(
  implicit annotationDataService: AnnotationDataService,
  config: TaskConfig
) extends Actor with StrictLogging {

  import config._
  import QASRLGenerationAccuracyManager._
  import QASRLSettings._

  val workerStatsFilename = "generationWorkerStats"

  var allWorkerStats =
    annotationDataService.loadLiveData(workerStatsFilename)
      .map(_.mkString)
      .map(read[Map[String, WorkerStats]])
      .toOption.getOrElse {
      // TODO assemble from saved data
      Map.empty[String, WorkerStats]
    }

  private[this] def save = {
    annotationDataService.saveLiveData(
      workerStatsFilename,
      write[Map[String, WorkerStats]](allWorkerStats))
    logger.info("Worker stats data saved.")
  }

  override def receive = {
    case SaveData => save
    case vr: QASRLValidationResult[SID] => vr match {
      case QASRLValidationResult(prompt, hitTypeId, hitId, assignmentId, numQAsValid) =>
        val ha = for {
          hit <- hitDataService.getHIT[GenerationPrompt[SID]](hitTypeId, hitId).toOptionPrinting.toList
          assignment <- hitDataService.getAssignmentsForHIT[List[VerbQA]](hitTypeId, hitId).get
          if assignment.assignmentId == assignmentId
        } yield (hit, assignment)

        ha.foreach { case (hit, assignment) =>
          // award bonuses
          val numSpecialWords = prompt.keywords.size
          val numQAsProvided = assignment.response.size
          val bonusAwarded = generationBonus(numQAsValid)
          if(bonusAwarded > 0.0) {
            service.grantBonus(
              assignment.workerId, bonusAwarded, assignment.assignmentId,
              s"""$numQAsValid out of $numQAsProvided question-answer pairs were judged to be valid,
            ${dollarsToCents(bonusAwarded)}c.""")
          }

          val stats = allWorkerStats
            .get(assignment.workerId)
            .getOrElse(WorkerStats.empty(assignment.workerId))
            .addAssignment(assignment.response.size, numQAsValid,
                           assignment.submitTime - assignment.acceptTime,
                           QASRLSettings.generationReward + bonusAwarded)

          // update qualifications according to performance
          val newStats = stats.warnedAt match {
            case None =>
              // set soft qualification since no warning yet
              config.service.updateQualificationScore(
                genQualificationTypeId,
                assignment.workerId,
                math.ceil(100 * math.max(stats.accuracy, generationAccuracyBlockingThreshold)).toInt)
              if(stats.accuracy < generationAccuracyWarningThreshold &&
                   stats.numAssignmentsCompleted >= generationBufferBeforeWarning) {

                service.notifyWorkers(
                  "Notification (warning) regarding the question-answer task",
                  notificationEmailText(stats.accuracy),
                  Array(assignment.workerId))

                logger.info(s"Generation worker ${assignment.workerId} warned at ${stats.numAssignmentsCompleted} with accuracy ${stats.accuracy}")
                stats.warned

              } else stats
            case Some(numWhenWarned) =>
              if(stats.numAssignmentsCompleted - numWhenWarned >= generationBufferBeforeBlocking) {

                config.service.updateQualificationScore(
                  genQualificationTypeId,
                  assignment.workerId,
                  math.ceil(100 * stats.accuracy).toInt)

                if(math.ceil(stats.accuracy).toInt < generationAccuracyBlockingThreshold) {
                  logger.info(s"Generation worker ${assignment.workerId} DQ'd at ${stats.numAssignmentsCompleted} with accuracy ${stats.accuracy}")
                  stats.blocked
                } else stats

              } else {
                // set soft qualification since still in buffer zone
                config.service.updateQualificationScore(
                  genQualificationTypeId,
                  assignment.workerId,
                  math.ceil(100 * math.max(stats.accuracy, generationAccuracyBlockingThreshold)).toInt)
                stats
              }
          }
          allWorkerStats = allWorkerStats.updated(assignment.workerId, newStats)
        }
    }
  }

}

object QASRLGenerationAccuracyManager {
  def notificationEmailText(curAccuracy: Double) = {
    import QASRLSettings._
    val explanatoryText = if(curAccuracy < generationAccuracyBlockingThreshold) {
      s"""There will be a grace period of several more assignments (${generationBufferBeforeBlocking} more after this calculation was done), and after that, if your accuracy remains below ${math.round(generationAccuracyBlockingThreshold * 100).toInt}%, you will no longer qualify for the task. Note that your qualification value may not accurately reflect your accuracy: it will be prevented from going below ${math.round(generationAccuracyBlockingThreshold * 100).toInt} until the grace period is over."""
    } else {
      s"""If this drops below ${math.round(generationAccuracyBlockingThreshold * 100).toInt}%, you will no longer qualify for the task. There will be a grace period (${generationBufferBeforeBlocking} more assignments after this calculation was done) during which your qualification value will be prevented from dropping below ${math.round(generationAccuracyBlockingThreshold * 100).toInt}."""
    }
    val dropOrRemain = if(curAccuracy < generationAccuracyBlockingThreshold) "remain" else "drop"

    f"""
Of your question-answer pairs that have been reviewed so far, ${math.round(curAccuracy * 10000.0) / 100.0}%.2f%% were judged valid by validators. $explanatoryText%s

If you are not sure why your score is this low, we recommend reading over the examples in the instructions again. We are still iterating on the task design and we are not yet sure what the common sources of disagreement are. After this batch of HITs, we will reset everyone's qualification values and try again with changes to the instructions and interface.
""".trim
  }

}
