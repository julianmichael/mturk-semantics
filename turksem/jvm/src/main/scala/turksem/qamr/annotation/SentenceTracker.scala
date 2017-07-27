package turksem.qamr.annotation

import turksem._
import turksem.qamr._

import turkey._
import turkey.tasks._

import nlpdata.util._

import akka.actor.Actor

import upickle.default._

import com.typesafe.scalalogging.StrictLogging

sealed trait TrackingUpdate[SID]
case class GenerationFinished[SID](prompt: GenerationPrompt[SID]) extends TrackingUpdate[SID]
case class ValidationBegun[SID](prompt: ValidationPrompt[SID]) extends TrackingUpdate[SID]
case class ValidationFinished[SID](
  prompt: ValidationPrompt[SID],
  assignments: List[Assignment[List[ValidationAnswer]]]
) extends TrackingUpdate[SID]

class SentenceTracker[SID : Reader : Writer : HasTokens](
  genHITTypeId: String,
  valHITTypeId: String)(
  implicit config: TaskConfig,
  annotationDataService: AnnotationDataService,
  isStopword: IsStopword,
  settings: PipelineSettings
) extends Actor with StrictLogging {

  import settings._

  val finishedSentenceStatsFilename = "finishedSentenceStats"
  var finishedSentenceStats: List[SentenceStats[SID]] =
    annotationDataService.loadLiveData(finishedSentenceStatsFilename)
      .map(_.mkString)
      .map(read[List[SentenceStats[SID]]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      List.empty[SentenceStats[SID]]
    }

  val aggregateSentenceStatsFilename = "aggregateSentenceStats"
  var aggregateSentenceStats: AggregateSentenceStats =
    annotationDataService.loadLiveData(aggregateSentenceStatsFilename)
      .map(_.mkString)
      .map(read[AggregateSentenceStats])
      .toOption.getOrElse {
      AggregateSentenceStats.aggregate(finishedSentenceStats)
    }

  val sentenceStatusesFilename = "sentenceStatuses"
  var sentenceStatuses: Map[SID, SentenceStatus[SID]] =
    annotationDataService.loadLiveData(sentenceStatusesFilename)
      .map(_.mkString)
      .map(read[Map[SID, SentenceStatus[SID]]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      Map.empty[SID, SentenceStatus[SID]]
    }

  def save = {
    annotationDataService.saveLiveData(
      finishedSentenceStatsFilename,
      write[List[SentenceStats[SID]]](finishedSentenceStats))
    annotationDataService.saveLiveData(
      sentenceStatusesFilename,
      write[Map[SID, SentenceStatus[SID]]](sentenceStatuses))
    annotationDataService.saveLiveData(
      aggregateSentenceStatsFilename,
      write[AggregateSentenceStats](aggregateSentenceStats))
    logger.info("Sentence tracker saved.")
  }

  def processUpdate(id: SID, update: TrackingUpdate[SID]) = {
    val newStatus = {
      val res = sentenceStatuses
        .get(id)
        .getOrElse(emptyStatus(id))
      update match {
        case GenerationFinished(gPrompt) => res.withKeywords(gPrompt.keywords.toSet)
        case ValidationBegun(vPrompt) => res.beginValidation(vPrompt)
        case ValidationFinished(vPrompt, assignments) => res.finishValidation(vPrompt, assignments)
      }
    }

    if(newStatus.isFinished) {
      val newStats = makeStats(newStatus, genHITTypeId, valHITTypeId)
      finishedSentenceStats =  newStats :: finishedSentenceStats
      aggregateSentenceStats = aggregateSentenceStats.add(newStats)
      sentenceStatuses = sentenceStatuses - id
    } else {
      sentenceStatuses = sentenceStatuses.updated(id, newStatus)
    }
  }

  override def receive = {
    case SaveData => save
    case Pring => println("Sentence tracker pringed.")
    case u: TrackingUpdate[SID] => u match {
      case u @ GenerationFinished(gPrompt) => processUpdate(gPrompt.id, u)
      case u @ ValidationBegun(vPrompt) => processUpdate(vPrompt.genPrompt.id, u)
      case u @ ValidationFinished(vPrompt, _) => processUpdate(vPrompt.genPrompt.id, u)
    }
  }
}
