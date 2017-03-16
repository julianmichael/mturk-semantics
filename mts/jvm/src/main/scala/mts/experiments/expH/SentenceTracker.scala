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
      AggregateSentenceStats.aggregate(finishedSentenceStats, System.nanoTime() / 1000000L)
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
      val newStats = makeStats(newStatus)
      finishedSentenceStats =  newStats :: finishedSentenceStats
      aggregateSentenceStats = aggregateSentenceStats.add(newStats, newStats.completionTime)
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

  def makeStats(status: SentenceStatus)(implicit config: TaskConfig): SentenceStats = {
    val allValidations = status.finishedAssignments
    val id = status.id
    val sentence = getTokensForId(id)
    val allValHITIds = allValidations.map(_.hitId).toSet
    val valHITInfos = allValHITIds.toList
      .map(hitId => FileManager.getHITInfo[ValidationPrompt, List[ValidationAnswer]](valHITTypeId, hitId).get)
    val allGenHITIds = valHITInfos.map(_.hit.prompt.sourceHITId).toSet
    val genHITInfos = allGenHITIds.toList
      .map(hitId => FileManager.getHITInfo[GenerationPrompt, List[WordedQAPair]](genHITTypeId, hitId).get)
    val sentenceHITInfo = SentenceHITInfo(sentence, genHITInfos, valHITInfos)

    val alignedValidations = sentenceHITInfo.alignValidations
    val allKeywords = genHITInfos.map(_.hit.prompt.keywords).flatten.toSet
    val qaPairsEachKeywordPrompt = for {
      HITInfo(hit, assignments) <- genHITInfos
      assignment <- assignments
      keywordIndex <- hit.prompt.keywords.toList
    } yield assignment.response.filter(_.wordIndex == keywordIndex).size
    val qaPairsEachKeywordActual = for {
      keywordIndex <- allKeywords.toList
    } yield {
      val qaPairs = for {
        HITInfo(hit, assignments) <- genHITInfos
        assignment <- assignments
        wqa @ WordedQAPair(_, question, answerIndices) <- assignment.response
        wordsInQuestion = getWordsInQuestion(sentence, question)
        if (wordsInQuestion union answerIndices).contains(keywordIndex)
      } yield wqa
      qaPairs.size
    }
    val validationLatencies = for {
      HITInfo(_, assignments) <- genHITInfos
      assignment <- assignments
      validations = for {
        HITInfo(valHIT, valAssignments) <- valHITInfos
        if valHIT.prompt.sourceAssignmentId == assignment.assignmentId
      } yield valAssignments.map(_.submitTime).max
      completion = validations.max
    } yield ((completion - assignment.submitTime) / 1000L).toInt  // seconds

    val numQAPairs = genHITInfos.flatMap(_.assignments).flatMap(_.response).size
    val numValidQAPairs = alignedValidations
      .map(av => numValidQuestions(av.valAssignments.map(_.response)))
      .sum
    val completionTime = valHITInfos.flatMap(_.assignments).map(_.submitTime).max
    val genCost = alignedValidations.map(_.genCost).sum
    val valCost = alignedValidations.map(_.valCost).sum
    val genHITIds = genHITInfos.map(_.hit.hitId).toSet
    val valHITIds = valHITInfos.map(_.hit.hitId).toSet
    SentenceStats(
      id,
      allKeywords.size,
      numQAPairs,
      numValidQAPairs,
      qaPairsEachKeywordPrompt,
      qaPairsEachKeywordActual,
      validationLatencies,
      completionTime,
      genCost, valCost,
      genHITIds, valHITIds)
  }

  def emptyStatus(id: SentenceId) = {
    val sentence = getTokensForId(id)
    val allKeywords = sentence.indices
      .filter(i => !reallyUninterestingTokens.contains(sentence(i)))
      .toSet
    SentenceStatus(id, allKeywords, Set.empty[Int], Set.empty[ValidationPrompt], List.empty[Assignment[List[ValidationAnswer]]])
  }
}
