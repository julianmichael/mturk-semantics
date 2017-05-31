package mts
import akka.actor.Actor

import turkey._
import turkey.tasks._

import nlpdata.datasets.wiktionary.Inflections

import upickle.default._

import com.typesafe.scalalogging.StrictLogging

sealed trait TrackingUpdate
case class GenerationFinished(prompt: GenerationPrompt) extends TrackingUpdate
case class ValidationBegun(prompt: ValidationPrompt) extends TrackingUpdate
case class ValidationFinished(
  prompt: ValidationPrompt,
  assignments: List[Assignment[List[ValidationAnswer]]]
) extends TrackingUpdate

class SentenceTracker(
  genHITTypeId: String,
  valHITTypeId: String)(
  implicit config: TaskConfig,
  inflections: Inflections
) extends Actor with StrictLogging {

  val finishedSentenceStatsFilename = "finishedSentenceStats"
  var finishedSentenceStats: List[SentenceStats] =
    loadDataFile(finalExperimentName, finishedSentenceStatsFilename)
      .map(_.mkString)
      .map(read[List[SentenceStats]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      List.empty[SentenceStats]
    }

  val aggregateSentenceStatsFilename = "aggregateSentenceStats"
  var aggregateSentenceStats: AggregateSentenceStats =
    loadDataFile(finalExperimentName, aggregateSentenceStatsFilename)
      .map(_.mkString)
      .map(read[AggregateSentenceStats])
      .toOption.getOrElse {
      AggregateSentenceStats.aggregate(finishedSentenceStats)
    }

  val sentenceStatusesFilename = "sentenceStatuses"
  var sentenceStatuses: Map[SentenceId, SentenceStatus] =
    loadDataFile(finalExperimentName, sentenceStatusesFilename)
      .map(_.mkString)
      .map(read[Map[SentenceId, SentenceStatus]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      Map.empty[SentenceId, SentenceStatus]
    }

  def saveData = {
    saveDataFile(
      finalExperimentName,
      finishedSentenceStatsFilename,
      write[List[SentenceStats]](finishedSentenceStats))
    saveDataFile(
      finalExperimentName,
      sentenceStatusesFilename,
      write[Map[SentenceId, SentenceStatus]](sentenceStatuses))
    saveDataFile(
      finalExperimentName,
      aggregateSentenceStatsFilename,
      write[AggregateSentenceStats](aggregateSentenceStats))
    logger.info("Sentence tracker saved.")
  }

  def processUpdate(id: SentenceId, update: TrackingUpdate) = {
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
    case SaveData => saveData
    case Pring => println("Sentence tracker pringed.")
    case u @ GenerationFinished(gPrompt) => processUpdate(gPrompt.id, u)
    case u @ ValidationBegun(vPrompt) => processUpdate(vPrompt.genPrompt.id, u)
    case u @ ValidationFinished(vPrompt, _) => processUpdate(vPrompt.genPrompt.id, u)
  }
}
