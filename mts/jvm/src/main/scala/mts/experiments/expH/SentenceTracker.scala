package mts.experiments.expH
import akka.actor.Actor

import mts.core._
import mts.tasks._
import mts.datasets.ptb._
import mts.util._
import mts.experiments._
import mts.language._


import upickle.default._

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
  inflections: Inflections) extends Actor {

  val finishedSentenceStatsFilename = "finishedSentenceStats"
  var finishedSentenceStats: List[SentenceStats] =
    FileManager.loadDataFile(finalExperimentName, finishedSentenceStatsFilename)
      .map(_.mkString)
      .map(read[List[SentenceStats]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      List.empty[SentenceStats]
    }

  val aggregateSentenceStatsFilename = "aggregateSentenceStats"
  var aggregateSentenceStats: AggregateSentenceStats =
    FileManager.loadDataFile(finalExperimentName, aggregateSentenceStatsFilename)
      .map(_.mkString)
      .map(read[AggregateSentenceStats])
      .toOption.getOrElse {
      AggregateSentenceStats.aggregate(finishedSentenceStats)
    }

  val sentenceStatusesFilename = "sentenceStatuses"
  var sentenceStatuses: Map[SentenceId, SentenceStatus] =
    FileManager.loadDataFile(finalExperimentName, sentenceStatusesFilename)
      .map(_.mkString)
      .map(read[Map[SentenceId, SentenceStatus]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      Map.empty[SentenceId, SentenceStatus]
    }

  def saveData = {
    FileManager.saveDataFile(
      finalExperimentName,
      finishedSentenceStatsFilename,
      write[List[SentenceStats]](finishedSentenceStats))
    FileManager.saveDataFile(
      finalExperimentName,
      sentenceStatusesFilename,
      write[Map[SentenceId, SentenceStatus]](sentenceStatuses))
    FileManager.saveDataFile(
      finalExperimentName,
      aggregateSentenceStatsFilename,
      write[AggregateSentenceStats](aggregateSentenceStats))
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
    case u @ GenerationFinished(gPrompt) => processUpdate(gPrompt.id, u)
    case u @ ValidationBegun(vPrompt) => processUpdate(vPrompt.genPrompt.id, u)
    case u @ ValidationFinished(vPrompt, _) => processUpdate(vPrompt.genPrompt.id, u)
  }
}
