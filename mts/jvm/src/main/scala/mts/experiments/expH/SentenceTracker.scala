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
      AggregateSentenceStats.aggregate(finishedSentenceStats, System.nanoTime())
    }

  val sentenceStatusesFilename = "sentenceStatuses"
  var sentenceStatuses: Map[PTBSentencePath, SentenceStatus] =
    FileManager.loadDataFile(finalExperimentName, sentenceStatusesFilename)
      .map(_.mkString)
      .map(read[Map[PTBSentencePath, SentenceStatus]])
      .toOption.getOrElse {
      // TODO assemble from saved data?
      Map.empty[PTBSentencePath, SentenceStatus]
    }

  def saveData = {
    FileManager.saveDataFile(
      finalExperimentName,
      finishedSentenceStatsFilename,
      write[List[SentenceStats]](finishedSentenceStats))
    FileManager.saveDataFile(
      finalExperimentName,
      sentenceStatusesFilename,
      write[Map[PTBSentencePath, SentenceStatus]](sentenceStatuses))
    FileManager.saveDataFile(
      finalExperimentName,
      aggregateSentenceStatsFilename,
      write[AggregateSentenceStats](aggregateSentenceStats))
  }

  def processUpdate(path: PTBSentencePath, update: TrackingUpdate) = {
    val newStatus = {
      val res = sentenceStatuses
        .get(path)
        .getOrElse(emptyStatus(path))
      update match {
        case GenerationFinished(gPrompt) => res.withKeywords(gPrompt.keywords.toSet)
        case ValidationBegun(vPrompt) => res.beginValidation(vPrompt)
        case ValidationFinished(vPrompt, assignments) => res.finishValidation(vPrompt, assignments)
      }
    }

    if(newStatus.isFinished) {
      val newStats = makeStats(newStatus)
      finishedSentenceStats =  newStats :: finishedSentenceStats
      aggregateSentenceStats = aggregateSentenceStats.add(newStats, System.nanoTime())
      sentenceStatuses = sentenceStatuses - path
    } else {
      sentenceStatuses = sentenceStatuses.updated(path, newStatus)
    }
  }

  override def receive = {
    case SaveData => saveData
    case u @ GenerationFinished(gPrompt) => processUpdate(gPrompt.path, u)
    case u @ ValidationBegun(vPrompt) => processUpdate(vPrompt.genPrompt.path, u)
    case u @ ValidationFinished(vPrompt, _) => processUpdate(vPrompt.genPrompt.path, u)
  }

  def makeStats(status: SentenceStatus)(implicit config: TaskConfig): SentenceStats = {
    val allValidations = status.finishedAssignments
    val path = status.path
    val sentence = FileManager.getPTBSentence(path).get
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
    } yield ((completion - assignment.submitTime) / 1000000000L).toInt  // seconds

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
      path, sentence,
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

  def emptyStatus(path: PTBSentencePath) = {
    val sentence = getPTBTokens(path)
    val allKeywords = sentence.indices
      .filter(i => !reallyUninterestingTokens.contains(sentence(i)))
      .toSet
    SentenceStatus(path, allKeywords, Set.empty[Int], Set.empty[ValidationPrompt], List.empty[Assignment[List[ValidationAnswer]]])
  }
}
