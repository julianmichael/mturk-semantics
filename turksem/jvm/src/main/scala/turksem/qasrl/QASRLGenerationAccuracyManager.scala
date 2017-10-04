package turksem.qasrl

import turkey._
import turkey.tasks._

import turksem._
import turksem.util._
import turksem.qamr.Pring
import turksem.qamr.SaveData

import scala.collection.mutable
import scala.util.{Try, Success, Failure}

import upickle.default.Reader

import akka.actor.{Actor, ActorRef}

import com.amazonaws.services.mturk.model.AssignmentStatus
import com.amazonaws.services.mturk.model.HITStatus
import com.amazonaws.services.mturk.model.SendBonusRequest
import com.amazonaws.services.mturk.model.NotifyWorkersRequest
import com.amazonaws.services.mturk.model.AssociateQualificationWithWorkerRequest

import upickle.default._

import com.typesafe.scalalogging.StrictLogging

class QASRLGenerationAccuracyManager[SID : Reader : Writer](
  genDisqualificationTypeId: String)(
  implicit annotationDataService: AnnotationDataService,
  config: TaskConfig,
  settings: QASRLSettings
) extends Actor with StrictLogging {

  import config._

  val workerStatsFilename = "generationWorkerStats"

  var allWorkerStats =
    annotationDataService.loadLiveData(workerStatsFilename)
      .map(_.mkString)
      .map(read[Map[String, QASRLGenerationWorkerStats]])
      .toOption.getOrElse {
      Map.empty[String, QASRLGenerationWorkerStats]
    }

  def christenWorker(workerId: String, numAgreementsToAdd: Int) = {
    allWorkerStats = allWorkerStats.get(workerId).fold(allWorkerStats) { stats =>
      allWorkerStats.updated(workerId, stats.addBonusValids(numAgreementsToAdd))
    }
  }

  private[this] def save = {
    Try(
      annotationDataService.saveLiveData(
        workerStatsFilename,
        write[Map[String, QASRLGenerationWorkerStats]](allWorkerStats))
    ).toOptionLogging(logger).foreach(_ => logger.info("Worker stats data saved."))
  }

  override def receive = {
    case SaveData => save
    case ChristenWorker(workerId, numAgreementsToAdd) => christenWorker(workerId, numAgreementsToAdd)
    case vr: QASRLValidationResult[SID] => vr match {
      case QASRLValidationResult(prompt, hitTypeId, hitId, assignmentId, numQAsValid) =>
        val ha = for {
          hit <- hitDataService.getHIT[QASRLGenerationPrompt[SID]](hitTypeId, hitId).toOptionLogging(logger).toList
          assignment <- hitDataService.getAssignmentsForHIT[List[VerbQA]](hitTypeId, hitId).get
          if assignment.assignmentId == assignmentId
        } yield (hit, assignment)

        ha.foreach { case (hit, assignment) =>
          // award bonuses
          val numQAsProvided = assignment.response.size
          val bonusAwarded = settings.generationBonus(numQAsValid)
          val bonusCents = dollarsToCents(bonusAwarded)
          if(bonusAwarded > 0.0) {
            Try(
              service.sendBonus(
                new SendBonusRequest()
                  .withWorkerId(assignment.workerId)
                  .withBonusAmount(f"$bonusAwarded%.2f")
                  .withAssignmentId(assignment.assignmentId)
                  .withReason(
                  s"""$numQAsValid out of $numQAsProvided question-answer pairs were judged to be valid, for a bonus of ${bonusCents}c."""))
            ).toOptionLogging(logger).ifEmpty(logger.error(s"Failed to grant bonus of $bonusCents to worker ${assignment.workerId}"))
          }

          val stats = allWorkerStats
            .get(assignment.workerId)
            .getOrElse(QASRLGenerationWorkerStats.empty(assignment.workerId))
            .addAssignment(assignment.response.size, numQAsValid,
                           assignment.submitTime - assignment.acceptTime,
                           settings.generationReward + bonusAwarded)

          if(stats.accuracy < settings.generationAccuracyBlockingThreshold &&
               stats.numAssignmentsCompleted > settings.generationAccuracyGracePeriod) {
            Try(
              config.service.associateQualificationWithWorker(
                new AssociateQualificationWithWorkerRequest()
                  .withQualificationTypeId(genDisqualificationTypeId)
                  .withWorkerId(assignment.workerId)
                  .withIntegerValue(1)
                  .withSendNotification(true))
            )
          }

          allWorkerStats = allWorkerStats.updated(assignment.workerId, stats)
        }
    }
  }
}
